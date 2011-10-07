pragma Check_Policy (Trace, Off);
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Memory;
with System.Native_Stack;
with System.Once;
with System.Secondary_Stack;
with System.Shared_Locking;
with System.Soft_Links;
with System.Storage_Elements;
with System.Tasking.Yield;
with System.Unwind;
with C.errno;
with C.signal;
with C.sys.signal;
package body System.Tasking.Inside is
   pragma Suppress (All_Checks);
   use type C.signed_int;
   use type C.signed_long;
   use type C.void_ptr;

   Abort_Checking_Span : constant Duration := 1.0;

   type Word is mod 2 ** Standard'Word_Size;

   function sync_bool_compare_and_swap (
      A1 : not null access Activation_State;
      A2 : Activation_State;
      A3 : Activation_State)
      return Boolean;
   function sync_bool_compare_and_swap (
      A1 : not null access Termination_State;
      A2 : Termination_State;
      A3 : Termination_State)
      return Boolean;
   pragma Import (Intrinsic, sync_bool_compare_and_swap,
      "__sync_bool_compare_and_swap_1");

   function sync_add_and_fetch (
      A1 : not null access Counter;
      A2 : Counter)
      return Counter;
   pragma Import (Intrinsic, sync_add_and_fetch, "__sync_add_and_fetch_4");

   Report_Traceback : access procedure (
      Current : Ada.Exceptions.Exception_Occurrence);
   pragma Import (Ada, Report_Traceback, "__drake_ref_report_traceback");
   pragma Weak_External (Report_Traceback);

   --  delay statement

   function "+" (Left : Native_Time.Native_Time; Right : Duration)
      return Native_Time.Native_Time;
   function "+" (Left : Native_Time.Native_Time; Right : Duration)
      return Native_Time.Native_Time is
   begin
      return Native_Time.To_Native_Time (
         Native_Time.To_Time (Left) + Right);
   end "+";

   procedure Delay_For (D : Duration);
   procedure Delay_For (D : Duration) is
      M : Mutex;
      C : Condition_Variable;
      Notified : Boolean;
      Aborted : Boolean;
   begin
      Enable_Abort;
      Initialize (M);
      Enter (M);
      Wait (
         C,
         M,
         Timeout => Duration'Max (D, 0.0),
         Notified => Notified,
         Aborted => Aborted);
      Leave (M);
      Finalize (M);
      Disable_Abort (Aborted);
   end Delay_For;

   procedure Delay_Until (T : Native_Time.Native_Time);
   procedure Delay_Until (T : Native_Time.Native_Time) is
      M : Mutex;
      C : Condition_Variable;
      Notified : Boolean;
      Aborted : Boolean;
   begin
      Enable_Abort;
      Initialize (M);
      Enter (M);
      Wait (
         C,
         M,
         Timeout => T,
         Notified => Notified,
         Aborted => Aborted);
      Leave (M);
      Finalize (M);
      Disable_Abort (Aborted);
   end Delay_Until;

   --  shared lock

   Shared_Lock : Mutex := (Handle => <>); -- uninitialized

   procedure Shared_Lock_Enter;
   procedure Shared_Lock_Enter is
   begin
      Enter (Shared_Lock);
   end Shared_Lock_Enter;

   procedure Shared_Lock_Leave;
   procedure Shared_Lock_Leave is
   begin
      Leave (Shared_Lock);
   end Shared_Lock_Leave;

   --  attributes

   generic
      type Element_Type is private;
      type Array_Type is array (Natural) of Element_Type;
      type Array_Access is access Array_Type;
   package Simple_Vectors is

      procedure Expand (
         Data : in out Array_Access;
         Length : in out Natural;
         New_Length : Natural;
         New_Item : Element_Type);

      procedure Clear (Data : in out Array_Access; Length : in out Natural);

   end Simple_Vectors;

   package body Simple_Vectors is

      procedure Expand (
         Data : in out Array_Access;
         Length : in out Natural;
         New_Length : Natural;
         New_Item : Element_Type)
      is
         function Cast is new Ada.Unchecked_Conversion (Address, Array_Access);
      begin
         if New_Length > Length then
            Data := Cast (Memory.Reallocate (
               Data.all'Address,
               Storage_Elements.Storage_Count (
                  Array_Type'Component_Size * New_Length)));
            for I in Length .. New_Length - 1 loop
               Data (I) := New_Item;
            end loop;
            Length := New_Length;
         end if;
      end Expand;

      procedure Clear (Data : in out Array_Access; Length : in out Natural) is
      begin
         Memory.Free (Data.all'Address);
         Data := null;
         Length := 0;
      end Clear;

   end Simple_Vectors;

   package Attribute_Vectors is new Simple_Vectors (
      Attribute,
      Attribute_Array,
      Attribute_Array_Access);

   function To_Address is new Ada.Unchecked_Conversion (C.void_ptr, Address);

   procedure Clear_Attributes (Item : Task_Id);
   procedure Clear_Attributes (Item : Task_Id) is
   begin
      for I in 0 .. Item.Attributes_Length - 1 loop
         declare
            A : Attribute renames Item.Attributes (I);
         begin
            if A.Index /= null then
               Clear (Item, A.Index.all);
            end if;
         end;
      end loop;
      Attribute_Vectors.Clear (Item.Attributes, Item.Attributes_Length);
   end Clear_Attributes;

   Attribute_Indexes_Lock : Mutex := (Handle => <>); -- uninitialized

   type Attribute_Index_Set is array (Natural) of Word;
   pragma Suppress_Initialization (Attribute_Index_Set);
   type Attribute_Index_Set_Access is access Attribute_Index_Set;

   Attribute_Indexes : Attribute_Index_Set_Access := null;
   Attribute_Indexes_Length : Natural := 0;

   package Attribute_Index_Sets is new Simple_Vectors (
      Word,
      Attribute_Index_Set,
      Attribute_Index_Set_Access);

   --  task record

   package Task_Record_Conv is new Address_To_Named_Access_Conversions (
      Task_Record,
      Task_Id);

   procedure Append_To_Completion_List (
      Master : Master_Access;
      Item : Task_Id);
   procedure Append_To_Completion_List (
      Master : Master_Access;
      Item : Task_Id) is
   begin
      Enter (Master.Mutex);
      if Master.List = null then
         --  making a ringed list
         Item.Previous_At_Same_Level := Item;
         Item.Next_At_Same_Level := Item;
         Master.List := Item;
      else
         --  append to last
         Item.Previous_At_Same_Level := Master.List.Previous_At_Same_Level;
         Item.Previous_At_Same_Level.Next_At_Same_Level := Item;
         Item.Next_At_Same_Level := Master.List;
         Item.Next_At_Same_Level.Previous_At_Same_Level := Item;
      end if;
      Item.Master_Of_Parent := Master;
      Leave (Master.Mutex);
   end Append_To_Completion_List;

   procedure Remove_From_Completion_List_No_Sync (Item : Task_Id);
   procedure Remove_From_Completion_List_No_Sync (Item : Task_Id) is
   begin
      if Item = Item.Master_Of_Parent.List then
         --  first-in, first-out
         if Item = Item.Next_At_Same_Level then
            Item.Master_Of_Parent.List := null;
            goto Cleared;
         else
            Item.Master_Of_Parent.List := Item.Next_At_Same_Level;
         end if;
      end if;
      Item.Previous_At_Same_Level.Next_At_Same_Level :=
         Item.Next_At_Same_Level;
      Item.Next_At_Same_Level.Previous_At_Same_Level :=
         Item.Previous_At_Same_Level;
   <<Cleared>>
      Item.Master_Of_Parent := null;
      Item.Previous_At_Same_Level := null;
      Item.Next_At_Same_Level := null;
   end Remove_From_Completion_List_No_Sync;

   procedure Remove_From_Completion_List (Item : Task_Id);
   procedure Remove_From_Completion_List (Item : Task_Id) is
   begin
      if Item.Master_Of_Parent /= null then
         declare
            Mutex_Ref : constant not null access Mutex :=
               Item.Master_Of_Parent.Mutex'Access;
         begin
            Enter (Mutex_Ref.all);
            Remove_From_Completion_List_No_Sync (Item);
            Leave (Mutex_Ref.all);
         end;
      end if;
   end Remove_From_Completion_List;

   procedure Free (Item : in out Task_Id);
   procedure Free (Item : in out Task_Id) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (
         String,
         String_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (
         String,
         Entry_Name_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (
         Rendezvous_Record,
         Rendezvous_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (
         Task_Record,
         Task_Id);
   begin
      --  detach from master
      Remove_From_Completion_List (Item);
      --  free attributes
      Clear_Attributes (Item);
      --  free task record
      if Item.Rendezvous /= null then
         Finalize (Item.Rendezvous.Calling);
         if Item.Rendezvous.To_Deallocate_Names then
            for I in Item.Rendezvous.Names'Range loop
               Unchecked_Free (Item.Rendezvous.Names (I));
            end loop;
         end if;
         Unchecked_Free (Item.Rendezvous);
      end if;
      Unchecked_Free (Item.Name);
      Unchecked_Free (Item);
   end Free;

   --  thead id

   TLS_Current_Task_Id : Task_Id := null;
   pragma Thread_Local_Storage (TLS_Current_Task_Id);

   Main_Task_Record : aliased Task_Record (Main);

   --  secondary stack and exception occurrence

   TLS_Stack : Soft_Links.Task_Local_Storage_Access := null;
   pragma Thread_Local_Storage (TLS_Stack);

   function Get_SS return not null Soft_Links.Task_Local_Storage_Access;
   function Get_SS return not null Soft_Links.Task_Local_Storage_Access is
   begin
      return TLS_Stack;
   end Get_SS;

   function Get_CE return Ada.Exceptions.Exception_Occurrence_Access;
   function Get_CE return Ada.Exceptions.Exception_Occurrence_Access is
      function Cast is new Ada.Unchecked_Conversion (
         Unwind.Exception_Occurrence_Access,
         Ada.Exceptions.Exception_Occurrence_Access);
   begin
      return Cast (Get_SS.Current_Exception'Access);
   end Get_CE;

   --  signal handler

   Old_SIGTERM_Action : aliased C.sys.signal.struct_sigaction :=
      (others => <>); --  uninitialized

   procedure Restore_SIGTERM_Handler;
   procedure Restore_SIGTERM_Handler is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.signal.sigaction (
         C.sys.signal.SIGTERM,
         Old_SIGTERM_Action'Access,
         null);
   end Restore_SIGTERM_Handler;

   procedure SIGTERM_Handler (
      Signal_Number : C.signed_int;
      Info : access C.sys.signal.struct_siginfo;
      Context : C.void_ptr);
   pragma Convention (C, SIGTERM_Handler);
   procedure SIGTERM_Handler (
      Signal_Number : C.signed_int;
      Info : access C.sys.signal.struct_siginfo;
      Context : C.void_ptr)
   is
      pragma Unreferenced (Info);
      pragma Unreferenced (Context);
      T : constant Task_Id := TLS_Current_Task_Id;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      --  pragma Check (Trance, Ada.Debug.Put (Name (T)));
      T.Aborted := True;
      if T.Abort_Handler /= null then
         T.Abort_Handler (T);
      end if;
      if T.Kind = Main then
         Restore_SIGTERM_Handler;
         Dummy := C.pthread.pthread_kill (T.Handle, Signal_Number);
      end if;
   end SIGTERM_Handler;

   procedure Set_SIGTERM_Handler;
   procedure Set_SIGTERM_Handler is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
      act : aliased C.sys.signal.struct_sigaction :=
         (others => <>); --  uninitialized
   begin
      act.sigaction_u.sa_sigaction := SIGTERM_Handler'Access;
      act.sa_flags := -- C.sys.signal.SA_NODEFER +
         C.sys.signal.SA_RESTART +
         C.sys.signal.SA_SIGINFO;
      Dummy := C.signal.sigemptyset (act.sa_mask'Access);
      Dummy := C.signal.sigaction (
         C.sys.signal.SIGTERM,
         act'Access,
         Old_SIGTERM_Action'Access);
   end Set_SIGTERM_Handler;

   procedure Mask_SIGTERM (How : C.signed_int);
   procedure Mask_SIGTERM (How : C.signed_int) is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
      Mask : aliased C.sys.signal.sigset_t;
   begin
      Dummy := C.signal.sigemptyset (Mask'Access);
      Dummy := C.signal.sigaddset (Mask'Access, C.sys.signal.SIGTERM);
      Dummy := C.pthread.pthread_sigmask (How, Mask'Access, null);
   end Mask_SIGTERM;

   --  registration

   Registered : Boolean := False;
   pragma Atomic (Registered);

   procedure Unregister;
   procedure Unregister is
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      --  main thread id
      if Main_Task_Record.Master_Top /= null then
         pragma Assert (Main_Task_Record.Master_Top.Within =
            Library_Task_Level);
         Leave_Master;
         pragma Assert (Main_Task_Record.Master_Top = null);
      end if;
      Clear_Attributes (Main_Task_Record'Access);
      TLS_Current_Task_Id := null;
      --  shared lock
      Finalize (Shared_Lock);
      Shared_Locking.Enter_Hook := Shared_Locking.Nop'Access;
      Shared_Locking.Leave_Hook := Shared_Locking.Nop'Access;
      --  once
      Once.Yield_Hook := Once.Nop'Access;
      --  delay statement
      Native_Time.Delay_For_Hook := Native_Time.Simple_Delay_For'Access;
      Native_Time.Delay_Until_Hook := Native_Time.Simple_Delay_Until'Access;
      --  attribute indexes
      Finalize (Attribute_Indexes_Lock);
      Attribute_Index_Sets.Clear (Attribute_Indexes, Attribute_Indexes_Length);
      --  secondary stack and exception occurrenc
      Soft_Links.Get_Task_Local_Storage :=
         Soft_Links.Get_Main_Task_Local_Storage'Access;
      Soft_Links.Get_Current_Excep := Soft_Links.Get_Main_Current_Excep'Access;
      --  signal handler
      Restore_SIGTERM_Handler;
      --  clear
      Registered := False;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Unregister;

   procedure Register;
   procedure Register is
   begin
      if not Registered then
         pragma Check (Trace, Ada.Debug.Put ("enter"));
         --  still it is single thread
         Registered := True;
         Termination.Register_Exit (Unregister'Access);
         --  shared lock
         Shared_Lock.Handle := C.pthread.PTHREAD_MUTEX_INITIALIZER;
         Shared_Locking.Enter_Hook := Shared_Lock_Enter'Access;
         Shared_Locking.Leave_Hook := Shared_Lock_Leave'Access;
         --  once
         Once.Yield_Hook := Yield'Access;
         --  delay statement
         Native_Time.Delay_For_Hook := Delay_For'Access;
         Native_Time.Delay_Until_Hook := Delay_Until'Access;
         --  attribute indexes
         Attribute_Indexes_Lock.Handle := C.pthread.PTHREAD_MUTEX_INITIALIZER;
         --  secondary stack and exception occurrence
         TLS_Stack := Soft_Links.Get_Main_Task_Local_Storage;
         Soft_Links.Get_Task_Local_Storage := Get_SS'Access;
         Soft_Links.Get_Current_Excep := Get_CE'Access;
         --  main thread id
         TLS_Current_Task_Id := Main_Task_Record'Access;
         Main_Task_Record.Handle := C.pthread.pthread_self;
         Main_Task_Record.Aborted := False;
         Main_Task_Record.Attributes := null;
         Main_Task_Record.Attributes_Length := 0;
         Main_Task_Record.Activation_State := AS_Active;
         Main_Task_Record.Termination_State := TS_Active;
         Main_Task_Record.Master_Level := Environment_Task_Level;
         Main_Task_Record.Master_Top := null; -- start from Library_Task_Level
         --  signal handler
         Set_SIGTERM_Handler;
         pragma Check (Trace, Ada.Debug.Put ("leave"));
      end if;
   end Register;

   --  thread body

   procedure Report (
      T : not null Task_Id;
      Current : Ada.Exceptions.Exception_Occurrence);
   procedure Report (
      T : not null Task_Id;
      Current : Ada.Exceptions.Exception_Occurrence)
   is
      function Cast is new Ada.Unchecked_Conversion (
         Ada.Exceptions.Exception_Occurrence,
         Unwind.Exception_Occurrence);
      subtype Fixed_String is String (Positive);
      Full_Name : Fixed_String;
      for Full_Name'Address use Cast (Current).Id.Full_Name;
   begin
      Termination.Error_New_Line;
      if Cast (Current).Num_Tracebacks > 0
         and then Report_Traceback'Address /= Null_Address
      then
         if T.Name = null then
            Termination.Error_Put ("One task");
         else
            Termination.Error_Put (T.Name.all);
         end if;
         Termination.Error_Put (" terminated by unhandled exception");
         Termination.Error_New_Line;
         Report_Traceback (Current);
      else
         Termination.Error_Put ("in ");
         if T.Name = null then
            Termination.Error_Put ("one task");
         else
            Termination.Error_Put (T.Name.all);
         end if;
         Termination.Error_Put (", raised ");
         Termination.Error_Put (
            Full_Name (1 .. Cast (Current).Id.Name_Length));
         if Cast (Current).Msg_Length > 0 then
            Termination.Error_Put (" : ");
            Termination.Error_Put (
               Cast (Current).Msg (1 .. Cast (Current).Msg_Length));
         end if;
         Termination.Error_New_Line;
      end if;
   end Report;

   function Thread (Rec : C.void_ptr) return C.void_ptr;
   pragma Convention (C, Thread);
   function Thread (Rec : C.void_ptr) return C.void_ptr is
      Result : C.void_ptr;
      Local : aliased Soft_Links.Task_Local_Storage;
      T : Task_Id := Task_Record_Conv.To_Pointer (To_Address (Rec));
      No_Detached : Boolean;
   begin
      TLS_Current_Task_Id := T;
      --  block SIGTERM
      Mask_SIGTERM (C.sys.signal.SIG_BLOCK);
      --  setup secondary stack
      Local.Secondary_Stack := Null_Address;
      Local.Current_Exception.Private_Data := Null_Address;
      TLS_Stack := Local'Unchecked_Access;
      --  setup signal stack
      Termination.Set_Signal_Stack (T.Signal_Stack'Access);
      --  execute
      declare
         procedure On_Exception;
         procedure On_Exception is
            Aborted : Boolean; -- ignored
         begin
            if T.Activation_State <= AS_Activating then
               --  an exception was raised until calling Accept_Activation
               T.Activation_Chain.Error := Any_Exception;
               Accept_Activation (Aborted => Aborted);
            end if;
         end On_Exception;
      begin
         T.Process (T.Params);
      exception
         when Standard'Abort_Signal =>
            On_Exception;
         when E : others =>
            Report (T, E);
            On_Exception;
      end;
      --  cancel calling queue
      Cancel_Calls;
      --  deactivate and set 'Terminated to False
      No_Detached := sync_bool_compare_and_swap (
         T.Termination_State'Access,
         TS_Active,
         TS_Terminated);
      --  free
      if No_Detached then
         Result := Rec; -- caller or master may wait
      else -- detached
         if T.Master_Of_Parent /= null then
            declare
               Mutex_Ref : constant not null access Mutex :=
                  T.Master_Of_Parent.Mutex'Access;
            begin
               Enter (Mutex_Ref.all);
               if T.Auto_Detach then
                  Remove_From_Completion_List_No_Sync (T);
                  if C.pthread.pthread_detach (T.Handle) /= 0 then
                     null; -- should be report ?
                  end if;
               end if;
               Leave (Mutex_Ref.all);
            end;
            if not T.Auto_Detach then
               Result := Rec; -- master already has been waiting
            else
               Free (T);
               Result := C.void_ptr (Null_Address);
            end if;
         else
            Free (T);
            Result := C.void_ptr (Null_Address);
         end if;
      end if;
      --  cleanup secondary stack
      Secondary_Stack.Clear;
      --  return
      return Result;
   end Thread;

   type Execution_Error is (None, Aborted, Elaboration_Error, Done);
   pragma Discard_Names (Execution_Error);

   procedure Execute (T : Task_Id; Error : out Execution_Error);
   procedure Execute (T : Task_Id; Error : out Execution_Error) is
   begin
      if not sync_bool_compare_and_swap (
         T.Activation_State'Access,
         AS_Suspended,
         AS_Activating)
      then
         Error := Done;
      elsif T.Aborted then -- aborted before activation
         Error := Aborted;
         T.Activation_State := AS_Error;
         T.Termination_State := TS_Terminated; -- C9A004A
      else
         if C.pthread.pthread_create (
            T.Handle'Access,
            null,
            Thread'Access,
            C.void_ptr (Task_Record_Conv.To_Address (T))) /= 0
         then
            Error := Elaboration_Error;
            T.Activation_State := AS_Error;
         else
            Error := None;
         end if;
      end if;
   end Execute;

   procedure Wait (T : Task_Id; Free_Task_Id : out Task_Id);
   procedure Wait (T : Task_Id; Free_Task_Id : out Task_Id) is
      Rec : aliased C.void_ptr;
   begin
      if T.Activation_State = AS_Error then
         Free_Task_Id := T;
      else
         if C.pthread.pthread_join (T.Handle, Rec'Access) /= 0 then
            raise Tasking_Error;
         end if;
         Free_Task_Id := Task_Record_Conv.To_Pointer (To_Address (Rec));
      end if;
   end Wait;

   procedure Send_Abort_Signal (T : Task_Id);
   procedure Send_Abort_Signal (T : Task_Id) is
   begin
      --  pragma Check (Trace, Ada.Debug.Put ("abort " & Name (T)));
      case C.pthread.pthread_kill (T.Handle, C.sys.signal.SIGTERM) is
         when 0 =>
            Yield;
         when C.errno.ESRCH =>
            pragma Assert (Terminated (T));
            null; -- no exception when it is already terminated, C9A003A
         when others =>
            raise Tasking_Error;
      end case;
   end Send_Abort_Signal;

   procedure Abort_Handler_On_Leave_Master (T : Task_Id);
   procedure Abort_Handler_On_Leave_Master (T : Task_Id) is
      M : constant Master_Access := T.Master_Top;
   begin
      --  Enter (M.Mutex);
      Send_Abort_Signal (M.List);
      --  Leave (M.Mutex);
   end Abort_Handler_On_Leave_Master;

   --  activation

   package Activation_Chain_Conv is new Address_To_Named_Access_Conversions (
      Activation_Chain_Data,
      Activation_Chain_Access);

   procedure Remove_From_Merged_Activation_Chain_List (
      C : not null Activation_Chain_Access);
   procedure Remove_From_Merged_Activation_Chain_List (
      C : not null Activation_Chain_Access)
   is
      Chain : constant access Activation_Chain := C.Self;
   begin
      if Activation_Chain_Conv.To_Pointer (Chain.Data) = C then
         Chain.Data := Null_Address;
      else
         declare
            I : Activation_Chain_Access :=
               Activation_Chain_Conv.To_Pointer (Chain.Data);
         begin
            while I.Merged /= C loop
               I := I.Merged;
            end loop;
            I.Merged := C.Merged;
         end;
      end if;
   end Remove_From_Merged_Activation_Chain_List;

   procedure Release (C : in out Activation_Chain_Access);
   procedure Release (C : in out Activation_Chain_Access) is
      procedure Free (Item : in out Activation_Chain_Access);
      procedure Free (Item : in out Activation_Chain_Access) is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation (
            Activation_Chain_Data,
            Activation_Chain_Access);
      begin
         Finalize (Item.Mutex);
         Finalize (Item.Condition_Variable);
         Unchecked_Free (Item);
      end Free;
   begin
      if sync_add_and_fetch (C.Release_Count'Access, 1) > C.Task_Count then
         Free (C);
      end if;
   end Release;

   procedure Set_Active (T : not null Task_Id);
   procedure Set_Active (T : not null Task_Id) is
   begin
      if not Elaborated (T) then
         pragma Check (Trance, Ada.Debug.Put ("elab error in " & Name (T)));
         T.Activation_Chain.Error := Elaboration_Error;
      end if;
      T.Activation_State := AS_Active;
   end Set_Active;

   procedure Set_Active (C : not null Activation_Chain_Access);
   procedure Set_Active (C : not null Activation_Chain_Access) is
      I : Task_Id := C.List;
   begin
      while I /= null loop
         if I.Activation_State <= AS_Activating then
            Set_Active (I);
         end if;
         I := I.Next_Of_Activation_Chain;
      end loop;
   end Set_Active;

   procedure Activate (
      Chain : not null access Activation_Chain;
      Error : out Activation_Error;
      Aborted : out Boolean);
   procedure Activate (
      Chain : not null access Activation_Chain;
      Error : out Activation_Error;
      Aborted : out Boolean)
   is
      C : Activation_Chain_Access :=
         Activation_Chain_Conv.To_Pointer (Chain.Data);
   begin
      Error := None;
      Aborted := False;
      while C /= null loop
         pragma Assert (C.Self = Chain);
         Enter (C.Mutex);
         declare
            I : Task_Id := C.List;
            Error_On_Execute : Execution_Error;
         begin
            while I /= null loop
               Execute (I, Error_On_Execute);
               if Error_On_Execute /= None
                  and then Error_On_Execute /= Done
               then
                  C.Task_Count := C.Task_Count - 1;
                  if Error_On_Execute = Elaboration_Error then
                     Error := Elaboration_Error;
                  end if;
               end if;
               I.Activation_Chain_Living := False; -- will free in here
               I := I.Next_Of_Activation_Chain;
            end loop;
         end;
         C.Activated_Count := C.Activated_Count + 1;
         if C.Activated_Count > C.Task_Count then
            Set_Active (C);
            Notify_All (C.Condition_Variable);
         else
            loop
               Wait (C.Condition_Variable, C.Mutex);
               Aborted := Is_Aborted; -- is this check worthwhile?
               exit when C.Activated_Count > C.Task_Count or else Aborted;
            end loop;
         end if;
         Error := Activation_Error'Max (Error, C.Error);
         Remove_From_Merged_Activation_Chain_List (C);
         Leave (C.Mutex);
         --  cleanup
         declare
            Merged : constant Activation_Chain_Access := C.Merged;
         begin
            Release (C);
            C := Merged;
         end;
      end loop;
   end Activate;

   procedure Activate (T : Task_Id; Error : out Activation_Error);
   procedure Activate (T : Task_Id; Error : out Activation_Error) is
      Error_On_Execute : Execution_Error;
   begin
      Error := None;
      Execute (T, Error_On_Execute);
      if Error_On_Execute /= Done then
         declare
            C : constant Activation_Chain_Access := T.Activation_Chain;
         begin
            Enter (C.Mutex);
            if Error_On_Execute /= None then
               C.Task_Count := C.Task_Count - 1;
               if Error_On_Execute = Elaboration_Error then
                  Error := Elaboration_Error;
               end if;
            else
               Set_Active (T);
               Notify_All (C.Condition_Variable);
            end if;
            Error := Activation_Error'Max (Error, C.Error);
            Leave (C.Mutex);
         end;
      end if;
      --  note, this procedure does not free activation chain
      --  it must call above Activate taking chain
   end Activate;

   --  completion

   procedure Free (Item : in out Master_Access);
   procedure Free (Item : in out Master_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (
         Master_Record,
         Master_Access);
   begin
      Finalize (Item.Mutex);
      Unchecked_Free (Item);
   end Free;

   --  queue

   package Queue_Node_Conv is new Address_To_Named_Access_Conversions (
      Queue_Node,
      Queue_Node_Access);

   function Uncall_Filter (
      The_Node : not null Queue_Node_Access;
      Params : Address)
      return Boolean;
   function Uncall_Filter (
      The_Node : not null Queue_Node_Access;
      Params : Address)
      return Boolean is
   begin
      return The_Node = Queue_Node_Conv.To_Pointer (Params);
   end Uncall_Filter;

   procedure Peek_No_Sync (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Previous : in out Queue_Node_Access;
      Current : in out Queue_Node_Access);
   procedure Peek_No_Sync (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Previous : in out Queue_Node_Access;
      Current : in out Queue_Node_Access) is
   begin
      Item := null;
      Search : while Current /= null loop
         if Filter = null or else Filter (Current, Params) then
            if Previous /= null then
               Previous.Next := Current.Next;
            else
               Object.Head := Current.Next;
            end if;
            if Current = Object.Tail then
               Object.Tail := Previous;
            end if;
            Item := Current;
            exit Search;
         end if;
         Previous := Current;
         Current := Current.Next;
      end loop Search;
   end Peek_No_Sync;

   --  implementation

   function Current_Task_Id return Task_Id is
   begin
      Register;
      return TLS_Current_Task_Id;
   end Current_Task_Id;

   function Main_Task_Id return Task_Id is
   begin
      Register;
      return Main_Task_Record'Access;
   end Main_Task_Id;

   procedure Create (
      T : out Task_Id;
      Params : Address;
      Process : not null access procedure (Params : Address);
      Name : String := "";
      Chain : access Activation_Chain := null;
      Elaborated : access Boolean := null;
      Master : Master_Access := null;
      Entry_Last_Index : Task_Entry_Index := 0)
   is
      Name_Data : String_Access := null;
      Chain_Data : Activation_Chain_Access := null;
      Level : Master_Level := Library_Task_Level;
      Rendezvous : Rendezvous_Access := null;
      Error : Execution_Error;
   begin
      Register;
      --  name
      if Name'Length /= 0 then
         Name_Data := new String'(Name);
      end if;
      --  activation chain
      if Chain /= null then
         Chain_Data := Activation_Chain_Conv.To_Pointer (Chain.Data);
         if Chain_Data = null then
            Chain_Data := new Activation_Chain_Data'(
               List => null,
               Task_Count => 0,
               Activated_Count => 0,
               Release_Count => 0,
               Mutex => <>, -- default initializer
               Condition_Variable => <>, -- default initializer
               Error => None,
               Merged => null,
               Self => Chain);
            Chain.Data := Activation_Chain_Conv.To_Address (Chain_Data);
         end if;
      end if;
      --  master
      if Master /= null then
         Level := Master.Within;
         pragma Assert (Level /= Foreign_Task_Level + 2); -- ???
      end if;
      --  rendezvous
      if Entry_Last_Index > 0 then
         Rendezvous := new Rendezvous_Record'(
            Last_Index => Entry_Last_Index,
            Calling => <>, -- default initializer
            To_Deallocate_Names => False,
            Names => (others => null));
      end if;
      --  task record
      T := new Task_Record'(
         Kind => Sub,
         Handle => <>, -- uninitialized
         Aborted => False,
         Abort_Handler => null,
         Attributes => null,
         Attributes_Length => 0,
         Activation_State => AS_Suspended, -- unexecuted
         Termination_State => TS_Active,
         Master_Level => Level,
         Master_Top => null,
         Params => Params,
         Process => Process.all'Unrestricted_Access,
         Preferred_Free_Mode => Detach,
         Name => Name_Data,
         Activation_Chain => Chain_Data,
         Next_Of_Activation_Chain => null,
         Activation_Chain_Living => False,
         Elaborated => Elaborated,
         Master_Of_Parent => Master,
         Previous_At_Same_Level => null,
         Next_At_Same_Level => null,
         Auto_Detach => False,
         Rendezvous => Rendezvous,
         Signal_Stack => <>); -- uninitialized
      --  for master
      if Master /= null then
         --  append to the parent's master
         Append_To_Completion_List (Master, T);
      end if;
      --  for activation
      if Chain_Data /= null then
         --  apeend to activation chain
         T.Activation_Chain_Living := True;
         T.Next_Of_Activation_Chain := Chain_Data.List;
         Chain_Data.List := T;
         Chain_Data.Task_Count := Chain_Data.Task_Count + 1;
      else
         --  try to create
         Execute (T, Error);
         if Error /= None then
            if Master /= null then
               Remove_From_Completion_List (T); -- rollback
            end if;
            Free (T); -- and remove from parent's master
            raise Tasking_Error;
         else
            T.Activation_State := AS_Active;
         end if;
      end if;
   end Create;

   procedure Wait (T : in out Task_Id; Aborted : out Boolean) is
   begin
      if T /= null then
         pragma Assert (T.Kind = Sub);
         declare
            T2 : constant Task_Id := T;
            Free_Task_Id : Task_Id;
         begin
            T := null; -- clear before raising any exception
            Wait (T2, Free_Task_Id);
            if Free_Task_Id /= null then
               Free (Free_Task_Id);
            end if;
         end;
      end if;
      Aborted := Is_Aborted;
   end Wait;

   procedure Detach (T : in out Task_Id) is
   begin
      if T /= null then
         pragma Assert (T.Kind = Sub);
         if sync_bool_compare_and_swap (
            T.Termination_State'Access,
            TS_Active,
            TS_Detached)
         then
            if T.Master_Of_Parent /= null then
               Enter (T.Master_Of_Parent.Mutex);
               T.Auto_Detach := True;
               Leave (T.Master_Of_Parent.Mutex);
            else
               declare
                  Handle : constant C.pthread.pthread_t := T.Handle;
               begin
                  T := null;
                  if C.pthread.pthread_detach (Handle) /= 0 then
                     raise Tasking_Error;
                  end if;
               end;
            end if;
         else
            declare
               Aborted : Boolean; -- ignored
            begin
               Wait (T, Aborted => Aborted); -- release by caller
            end;
         end if;
      end if;
   end Detach;

   function Terminated (T : Task_Id) return Boolean is
   begin
      if T = null then
         raise Program_Error; -- RM C.7.1(15)
      elsif not Registered then -- already terminated main program
         return True;
      else
         return T.Termination_State = TS_Terminated;
      end if;
   end Terminated;

   function Activated (T : Task_Id) return Boolean is
   begin
      if T = null then
         raise Program_Error; -- RM C.7.1(15)
      elsif not Registered then -- already terminated main program
         return True;
      else
         return T.Activation_State = AS_Active;
      end if;
   end Activated;

   function Preferred_Free_Mode (T : not null Task_Id) return Free_Mode is
   begin
      return T.Preferred_Free_Mode;
   end Preferred_Free_Mode;

   procedure Set_Preferred_Free_Mode (
      T : not null Task_Id;
      Mode : Free_Mode) is
   begin
      T.Preferred_Free_Mode := Mode;
   end Set_Preferred_Free_Mode;

   procedure Get_Stack (
      T : not null Task_Id;
      Addr : out Address;
      Size : out Storage_Elements.Storage_Count) is
   begin
      Native_Stack.Get (T.Handle, Addr, Size);
   end Get_Stack;

   function Name (T : not null Task_Id) return String is
   begin
      if T.Kind = Main then
         return "*main";
      elsif T.Name = null then
         return "";
      else
         return T.Name.all;
      end if;
   end Name;

   procedure Send_Abort (T : not null Task_Id) is
      Current_Task_Id : constant Task_Id := TLS_Current_Task_Id;
   begin
      if T = Current_Task_Id then
         T.Aborted := True;
         raise Standard'Abort_Signal;
      elsif T.Activation_State = AS_Suspended then
         T.Aborted := True;
      else
         Send_Abort_Signal (T);
         T.Aborted := True; -- set 'Callable to false, C9A009H
         --  abort myself if parent task is aborted, C9A007A
         declare
            P : Task_Id := Current_Task_Id;
         begin
            loop
               P := Parent (P);
               exit when P = null;
               if P = T then
                  Current_Task_Id.Aborted := True;
                  raise Standard'Abort_Signal;
               end if;
            end loop;
         end;
      end if;
   end Send_Abort;

   procedure Enable_Abort is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      if T /= null and then T.Kind = Sub then
         Mask_SIGTERM (C.sys.signal.SIG_UNBLOCK);
      end if;
   end Enable_Abort;

   procedure Disable_Abort (Aborted : Boolean) is
      pragma Unreferenced (Aborted); -- dummy parameter for coding check
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      if T /= null and then T.Kind = Sub then
         Mask_SIGTERM (C.sys.signal.SIG_BLOCK);
      end if;
      if T /= null and then T.Aborted then
         raise Standard'Abort_Signal;
      end if;
   end Disable_Abort;

   function Is_Aborted return Boolean is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      return T /= null and then T.Aborted;
   end Is_Aborted;

   function Elaborated (T : not null Task_Id) return Boolean is
   begin
      return T.Elaborated = null or else T.Elaborated.all;
   end Elaborated;

   procedure Accept_Activation (Aborted : out Boolean) is
      T : constant Task_Id := TLS_Current_Task_Id;
      C : Activation_Chain_Access := T.Activation_Chain;
   begin
      pragma Assert (C /= null);
      Enter (C.Mutex);
      Aborted := T.Aborted;
      C.Activated_Count := C.Activated_Count + 1;
      if C.Activated_Count > C.Task_Count then
         Set_Active (C);
         Notify_All (C.Condition_Variable);
      else
         while T.Activation_State /= AS_Active and then not Aborted loop
            Wait (C.Condition_Variable, C.Mutex);
            Aborted := T.Aborted; -- is this check worthwhile?
         end loop;
      end if;
      --  elaboration error shold be delivered, but other exceptions should not
      Aborted := Aborted or else C.Error = Elaboration_Error;
      Leave (C.Mutex);
      --  cleanup
      Release (C);
   end Accept_Activation;

   procedure Activate (
      Chain : not null access Activation_Chain;
      Aborted : out Boolean)
   is
      Error : Activation_Error;
   begin
      Activate (Chain, Error, Aborted => Aborted);
      case Error is
         when None =>
            null;
         when Elaboration_Error =>
            raise Program_Error; -- C39008A, RM 3.11 (14)
         when Any_Exception =>
            raise Tasking_Error; -- C93004A
      end case;
   end Activate;

   procedure Activate (T : not null Task_Id)
   is
      Error : Activation_Error;
   begin
      Activate (T, Error);
      case Error is
         when None =>
            null;
         when Elaboration_Error =>
            raise Program_Error;
         when Any_Exception =>
            raise Tasking_Error;
      end case;
   end Activate;

   procedure Move (
      From, To : not null access Activation_Chain;
      New_Master : Master_Access) is
   begin
      --  note: keep master level of tasks because it's meaningless
      if From.Data /= Null_Address then
         --  change completion lists
         declare
            A : Activation_Chain_Access :=
               Activation_Chain_Conv.To_Pointer (From.Data);
         begin
            loop
               A.Self := To;
               declare
                  I : Task_Id := A.List;
               begin
                  while I /= null loop
                     if I.Master_Of_Parent /= null then
                        Remove_From_Completion_List (I);
                        Append_To_Completion_List (New_Master, I);
                     end if;
                     I := I.Next_Of_Activation_Chain;
                  end loop;
               end;
               A := A.Merged;
               exit when A = null;
            end loop;
         end;
         --  merge lists
         if To.Data = Null_Address then
            To.Data := From.Data;
         else
            declare
               I : Activation_Chain_Access :=
                  Activation_Chain_Conv.To_Pointer (To.Data);
            begin
               while I.Merged /= null loop
                  I := I.Merged;
               end loop;
               I.Merged := Activation_Chain_Conv.To_Pointer (From.Data);
            end;
         end if;
         From.Data := Null_Address;
      end if;
   end Move;

   function Parent (T : not null Task_Id) return Task_Id is
   begin
      if T.Master_Of_Parent = null then
         return null;
      else
         return T.Master_Of_Parent.Parent;
      end if;
   end Parent;

   function Master_Level_Of (T : not null Task_Id) return Master_Level is
   begin
      return T.Master_Level;
   end Master_Level_Of;

   function Master_Within return Master_Level is
      T : constant Task_Id := Current_Task_Id;
   begin
      if T.Master_Top = null then
         return T.Master_Level + 1;
      else
         return T.Master_Top.Within;
      end if;
   end Master_Within;

   procedure Enter_Master is
      T : constant Task_Id := Current_Task_Id; -- and register
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter in " & Name (T)));
      declare
         New_Master : constant Master_Access := new Master_Record'(
            Previous => T.Master_Top,
            Parent => T,
            Within => Master_Level'Max (Master_Within, Library_Task_Level) + 1,
            List => null,
            Mutex => <>); -- default initializer
      begin
         T.Master_Top := New_Master;
      end;
      pragma Check (Trace, Ada.Debug.Put ("leave in " & Name (T)));
   end Enter_Master;

   procedure Leave_Master is
      T : constant Task_Id := TLS_Current_Task_Id;
      pragma Check (Trace, Ada.Debug.Put ("enter in " & Name (T)));
      M : Master_Access := T.Master_Top;
      Free_List : Task_Id := null;
   begin
      Enter (M.Mutex);
      while M.List /= null loop
         declare
            Taken : constant Task_Id := M.List;
            Free_Task_Id : Task_Id;
            Error : Activation_Error; -- ignored
            Aborted : Boolean; -- ignored
         begin
            Taken.Auto_Detach := False; -- mark inside mutex
            Leave (M.Mutex);
            if Taken.Activation_State = AS_Suspended then
               pragma Assert (Taken.Activation_Chain /= null);
               --  the task has not been activated
               Taken.Activation_Chain.Error := Elaboration_Error;
               if Taken.Activation_Chain_Living then
                  Activate (
                     Taken.Activation_Chain.Self,
                     Error,
                     Aborted => Aborted);
               end if;
            elsif T.Aborted then
               Send_Abort_Signal (Taken); -- C9A007A
            end if;
            pragma Assert (Current_Task_Id.Abort_Handler = null);
            Current_Task_Id.Abort_Handler :=
               Abort_Handler_On_Leave_Master'Access;
            Mask_SIGTERM (C.sys.signal.SIG_UNBLOCK);
            Wait (Taken, Free_Task_Id);
            Mask_SIGTERM (C.sys.signal.SIG_BLOCK);
            Current_Task_Id.Abort_Handler := null;
            if Free_Task_Id /= null then
               Remove_From_Completion_List (Free_Task_Id);
               Free_Task_Id.Next_At_Same_Level := Free_List;
               Free_List := Free_Task_Id;
            end if;
            Enter (M.Mutex);
         end;
      end loop;
      Leave (M.Mutex);
      while Free_List /= null loop
         declare
            Next : constant Task_Id := Free_List.Next_At_Same_Level;
         begin
            Free (Free_List);
            Free_List := Next;
         end;
      end loop;
      declare
         Previous : constant Master_Access := M.Previous;
      begin
         Free (M);
         T.Master_Top := Previous;
      end;
      pragma Check (Trace, Ada.Debug.Put ("leave in " & Name (T)));
   end Leave_Master;

   procedure Leave_All_Masters is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      if T.Master_Top /= null then
         Leave_Master;
         pragma Assert (T.Master_Top = null);
      end if;
   end Leave_All_Masters;

   function Master_Of_Parent (Level : Master_Level) return Master_Access is
      Parent : Task_Id := TLS_Current_Task_Id;
      Result : Master_Access;
   begin
      while Parent.Master_Level >= Level loop
         pragma Assert (Parent.Kind = Sub);
         pragma Assert (Parent.Master_Of_Parent /= null);
         pragma Assert (Parent.Master_Of_Parent.Parent /= null);
         Parent := Parent.Master_Of_Parent.Parent;
      end loop;
      Result := Parent.Master_Top;
      while Result /= null and then Result.Within > Level loop
         Result := Result.Previous;
      end loop;
      if Result = null then
         --  library level
         pragma Assert (Parent = Main_Task_Record'Access);
         if Parent.Master_Top = null then
            Enter_Master;
            pragma Assert (Parent.Master_Top.Within = Library_Task_Level + 1);
            Parent.Master_Top.Within := Library_Task_Level;
         end if;
         pragma Assert (Parent.Master_Top.Within = Library_Task_Level);
         Result := Parent.Master_Top;
      end if;
      return Result;
   end Master_Of_Parent;

   procedure Set_Entry_Name (
      T : not null Task_Id;
      Index : Task_Entry_Index;
      Name : Entry_Name_Access) is
   begin
      T.Rendezvous.Names (Index) := Name;
   end Set_Entry_Name;

   procedure Set_Entry_Names_To_Deallocate (T : not null Task_Id) is
   begin
      if T.Rendezvous /= null then
         T.Rendezvous.To_Deallocate_Names := True;
      end if;
   end Set_Entry_Names_To_Deallocate;

   procedure Cancel_Calls is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      if T.Rendezvous /= null then
         Cancel (T.Rendezvous.Calling, Cancel_Node => Cancel_Call_Hook);
      end if;
   end Cancel_Calls;

   procedure Call (T : not null Task_Id; Item : not null Queue_Node_Access) is
   begin
      Add (T.Rendezvous.Calling, Item);
   end Call;

   procedure Uncall (
      T : not null Task_Id;
      Item : not null Queue_Node_Access;
      Already_Taken : out Boolean)
   is
      Taken : Queue_Node_Access;
   begin
      Take (
         T.Rendezvous.Calling,
         Taken,
         Queue_Node_Conv.To_Address (Item),
         Uncall_Filter'Access);
      Already_Taken := Taken = null;
      pragma Assert (Taken = null or else Taken = Item);
   end Uncall;

   procedure Accept_Call (
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Aborted : out Boolean)
   is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      Take (
         T.Rendezvous.Calling,
         Item,
         Params,
         Filter,
         Aborted);
   end Accept_Call;

   function Call_Count (
      T : not null Task_Id;
      Params : Address;
      Filter : Queue_Filter)
      return Natural is
   begin
      if not Callable (T) or else T.Rendezvous = null then
         return 0;
      else
         return Count (T.Rendezvous.Calling'Access, Params, Filter);
      end if;
   end Call_Count;

   function Callable (T : not null Task_Id) return Boolean is
   begin
      return not Terminated (T)
         and then T.Kind = Sub
         and then not T.Aborted
         and then (
            T.Rendezvous = null
            or else not Canceled (T.Rendezvous.Calling));
   end Callable;

   --  thread local storage

--  procedure Allocate (Index : out TLS_Index) is
--    Key : aliased C.pthread.pthread_key_t;
--  begin
--    if C.pthread.pthread_key_create (Key'Access, null) = 0 then
--       Index := TLS_Index (Key);
--    else
--       raise Tasking_Error;
--    end if;
--  end Allocate;

--  procedure Free (Index : in out TLS_Index) is
--  begin
--    if C.pthread.pthread_key_delete (
--       C.pthread.pthread_key_t (Index)) /= 0
--    then
--       null; -- raise Tasking_Error;
--    end if;
--  end Free;

--  function Get (Index : TLS_Index) return Address is
--  begin
--    return To_Address (C.pthread.pthread_getspecific (
--       C.pthread.pthread_key_t (Index)));
--  end Get;

--  procedure Set (Index : TLS_Index; Item : Address) is
--  begin
--    if C.pthread.pthread_setspecific (
--       C.pthread.pthread_key_t (Index),
--       C.void_const_ptr (Item)) /= 0
--    then
--       raise Tasking_Error;
--    end if;
--  end Set;

   --  attribute

   procedure Allocate (Index : in out Attribute_Index) is
   begin
      Enter (Attribute_Indexes_Lock);
      --  initialization because Suppress_Initialization
      Index.List := null;
      Index.Mutex.Handle := C.pthread.PTHREAD_MUTEX_INITIALIZER;
      --  search unused index
      for I in 0 .. Attribute_Indexes_Length - 1 loop
         if Attribute_Indexes (I) /= not 0 then
            for B in 0 .. Word'Size - 1 loop
               if (Attribute_Indexes (I) and (2 ** B)) = 0 then
                  Attribute_Indexes (I) := Attribute_Indexes (I) or (2 ** B);
                  Index.Index := I * Word'Size + B;
                  goto Found;
               end if;
            end loop;
         end if;
      end loop;
      --  not found
      Index.Index := Word'Size * Attribute_Indexes_Length;
      declare
         P : constant Natural := Attribute_Indexes_Length;
         B : constant Natural := 0;
      begin
         Attribute_Index_Sets.Expand (
            Attribute_Indexes,
            Attribute_Indexes_Length,
            P + 1,
            0);
         Attribute_Indexes (P) := Attribute_Indexes (P) or (2 ** B);
      end;
   <<Found>>
      Leave (Attribute_Indexes_Lock);
   end Allocate;

   procedure Free (Index : in out Attribute_Index) is
   begin
      if Registered then -- if not, already terminated main program
         Enter (Attribute_Indexes_Lock);
      end if;
      while Index.List /= null loop
         declare
            Next : constant Task_Id :=
               Index.List.Attributes (Index.Index).Next;
         begin
            declare
               A : Attribute renames Index.List.Attributes (Index.Index);
            begin
               A.Finalize (A.Item);
               A.Index := null;
            end;
            Index.List := Next;
         end;
      end loop;
      if Registered then
         declare
            P : constant Natural := Index.Index / Word'Size;
            B : constant Natural := Index.Index mod Word'Size;
         begin
            Attribute_Indexes (P) := Attribute_Indexes (P) and not (2 ** B);
         end;
      end if;
      Finalize (Index.Mutex);
      if Registered then
         Leave (Attribute_Indexes_Lock);
      end if;
   end Free;

   procedure Query (
      T : not null Task_Id;
      Index : in out Attribute_Index;
      Process : not null access procedure (Item : Address))
   is
      Value : Address;
   begin
      Enter (Index.Mutex);
      if T.Attributes_Length > Index.Index
         and then T.Attributes (Index.Index).Index = Index'Unrestricted_Access
      then
         Value := T.Attributes (Index.Index).Item;
      else
         Value := Null_Address;
      end if;
      Process (Value);
      Leave (Index.Mutex);
   end Query;

   procedure Set (
      T : not null Task_Id;
      Index : in out Attribute_Index;
      New_Item : not null access function return Address;
      Finalize : not null access procedure (Item : Address)) is
   begin
      Enter (Index.Mutex);
      Attribute_Vectors.Expand (
         T.Attributes,
         T.Attributes_Length,
         Index.Index + 1,
         Attribute'(Index => null, others => <>));
      declare
         A : Attribute renames T.Attributes (Index.Index);
      begin
         if A.Index = Index'Unrestricted_Access then
            A.Finalize (A.Item);
         end if;
         A.Item := New_Item.all;
         A.Finalize := Finalize.all'Access;
         if A.Index /= Index'Unrestricted_Access then
            A.Index := Index'Unrestricted_Access;
            A.Previous := null;
            A.Next := Index.List;
            if Index.List /= null then
               Index.List.Attributes (Index.Index).Previous := T;
            end if;
            Index.List := T;
         end if;
      end;
      Leave (Index.Mutex);
   end Set;

   procedure Reference (
      T : not null Task_Id;
      Index : in out Attribute_Index;
      New_Item : not null access function return Address;
      Finalize : not null access procedure (Item : Address);
      Result : out Address) is
   begin
      Enter (Index.Mutex);
      Attribute_Vectors.Expand (
         T.Attributes,
         T.Attributes_Length,
         Index.Index + 1,
         Attribute'(Index => null, others => <>));
      declare
         A : Attribute renames T.Attributes (Index.Index);
      begin
         if A.Index /= Index'Unrestricted_Access then
            A.Item := New_Item.all;
            A.Finalize := Finalize.all'Access;
            if A.Index /= Index'Unrestricted_Access then
               A.Index := Index'Unrestricted_Access;
               A.Previous := null;
               A.Next := Index.List;
               if Index.List /= null then
                  Index.List.Attributes (Index.Index).Previous := T;
               end if;
               Index.List := T;
            end if;
         end if;
         Result := A.Item;
      end;
      Leave (Index.Mutex);
   end Reference;

   procedure Clear (
      T : not null Task_Id;
      Index : in out Attribute_Index) is
   begin
      Enter (Index.Mutex);
      if T.Attributes_Length > Index.Index
         and then T.Attributes (Index.Index).Index = Index'Unrestricted_Access
      then
         declare
            A : Attribute renames T.Attributes (Index.Index);
         begin
            A.Finalize (A.Item);
            if A.Previous /= null then
               A.Previous.Attributes (Index.Index).Next := A.Next;
            else
               A.Index.List := A.Next;
            end if;
            if A.Next /= null then
               A.Next.Attributes (Index.Index).Previous := A.Previous;
            end if;
            A.Index := null;
         end;
      end if;
      Leave (Index.Mutex);
   end Clear;

   --  mutex

   procedure Finalize (Object : in out Mutex) is
   begin
      if C.pthread.pthread_mutex_destroy (Object.Handle'Access) /= 0 then
         null; -- raise Tasking_Error;
      end if;
   end Finalize;

   procedure Enter (Object : in out Mutex) is
   begin
      if C.pthread.pthread_mutex_lock (Object.Handle'Access) /= 0 then
         raise Tasking_Error;
      end if;
   end Enter;

   procedure Leave (Object : in out Mutex) is
   begin
      if C.pthread.pthread_mutex_unlock (Object.Handle'Access) /= 0 then
         raise Tasking_Error;
      end if;
   end Leave;

   --  condition variable

   procedure Finalize (Object : in out Condition_Variable) is
   begin
      if C.pthread.pthread_cond_destroy (Object.Handle'Access) /= 0 then
         null; -- raise Tasking_Error;
      end if;
   end Finalize;

   procedure Notify_One (Object : in out Condition_Variable) is
   begin
      if C.pthread.pthread_cond_signal (Object.Handle'Access) /= 0 then
         raise Tasking_Error;
      end if;
   end Notify_One;

   procedure Notify_All (Object : in out Condition_Variable) is
   begin
      if C.pthread.pthread_cond_broadcast (Object.Handle'Access) /= 0 then
         raise Tasking_Error;
      end if;
   end Notify_All;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Inside.Mutex) is
   begin
      if C.pthread.pthread_cond_wait (
         Object.Handle'Access,
         Mutex.Handle'Access) /= 0
      then
         raise Tasking_Error;
      end if;
   end Wait;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Inside.Mutex;
      Timeout : Native_Time.Native_Time;
      Notified : out Boolean) is
   begin
      case C.pthread.pthread_cond_timedwait (
         Object.Handle'Access,
         Mutex.Handle'Access,
         Timeout'Unrestricted_Access)
      is
         when 0 =>
            Notified := True;
         when C.errno.ETIMEDOUT =>
            Notified := False;
         when others =>
            raise Tasking_Error;
      end case;
   end Wait;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Inside.Mutex;
      Timeout : Duration;
      Notified : out Boolean) is
   begin
      Wait (
         Object,
         Mutex,
         Native_Time.Clock + Timeout,
         Notified);
   end Wait;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Inside.Mutex;
      Aborted : out Boolean) is
   begin
      loop
         Aborted := Is_Aborted;
         exit when Aborted;
         declare
            Notified : Boolean;
         begin
            Wait (Object, Mutex, Abort_Checking_Span, Notified);
            exit when Notified;
         end;
      end loop;
   end Wait;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Inside.Mutex;
      Timeout : Native_Time.Native_Time;
      Notified : out Boolean;
      Aborted : out Boolean) is
   begin
      Notified := False;
      Aborted := Is_Aborted;
      if not Aborted then
         declare
            N : Native_Time.Native_Time;
         begin
            loop
               N := Native_Time.Clock + Abort_Checking_Span;
               exit when Native_Time.To_Time (N)
                  >= Native_Time.To_Time (Timeout);
               Wait (Object, Mutex, N, Notified);
               Aborted := Is_Aborted;
               if Notified or else Aborted then
                  return;
               end if;
            end loop;
            Wait (Object, Mutex, Timeout, Notified);
            Aborted := Is_Aborted;
         end;
      end if;
   end Wait;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Inside.Mutex;
      Timeout : Duration;
      Notified : out Boolean;
      Aborted : out Boolean) is
   begin
      Notified := False;
      Aborted := Is_Aborted;
      if not Aborted then
         declare
            R : Duration := Timeout;
         begin
            while R > Abort_Checking_Span loop
               Wait (Object, Mutex, Abort_Checking_Span, Notified);
               Aborted := Is_Aborted;
               if Notified or else Aborted then
                  return;
               end if;
               R := R - Abort_Checking_Span;
            end loop;
            Wait (Object, Mutex, R, Notified);
            Aborted := Is_Aborted;
         end;
      end if;
   end Wait;

   --  queue

   procedure Finalize (Object : in out Queue) is
   begin
      Finalize (Object.Mutex);
      Finalize (Object.Condition_Variable);
   end Finalize;

   procedure Cancel (
      Object : in out Queue;
      Cancel_Node : access procedure (X : in out Queue_Node_Access)) is
   begin
      Enter (Object.Mutex);
      Object.Canceled := True;
      if Cancel_Node /= null then
         while Object.Head /= null loop
            declare
               Next : constant Queue_Node_Access := Object.Head.Next;
            begin
               Cancel_Node (Object.Head);
               Object.Head := Next;
            end;
         end loop;
      end if;
      Leave (Object.Mutex);
   end Cancel;

   procedure Add (
      Object : in out Queue;
      Item : not null Queue_Node_Access)
   is
      Error : Boolean;
   begin
      Enter (Object.Mutex);
      Error := Object.Canceled;
      if not Error then
         if Object.Head = null then
            Object.Head := Item;
         else
            Object.Tail.Next := Item;
         end if;
         Object.Tail := Item;
         Item.Next := null;
         if Object.Waiting
            and then (Object.Filter = null
               or else Object.Filter (Item, Object.Params))
         then
            Notify_All (Object.Condition_Variable);
         end if;
      end if;
      Leave (Object.Mutex);
      if Error then
         raise Tasking_Error;
      end if;
   end Add;

   procedure Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter) is
   begin
      Enter (Object.Mutex);
      declare
         Previous : Queue_Node_Access := null;
         I : Queue_Node_Access := Object.Head;
      begin
         Peek_No_Sync (Object, Item, Params, Filter, Previous, I);
      end;
      Leave (Object.Mutex);
   end Take;

   procedure Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Aborted : out Boolean) is
   begin
      Aborted := Is_Aborted;
      Enter (Object.Mutex);
      declare
         Previous : Queue_Node_Access := null;
         I : Queue_Node_Access := Object.Head;
      begin
         Taking : loop
            Peek_No_Sync (Object, Item, Params, Filter, Previous, I);
            exit Taking when Item /= null;
            Not_Found : declare
               Tail_On_Waiting : constant Queue_Node_Access := Object.Tail;
               Notified : Boolean; -- ignored
            begin
               Object.Params := Params;
               Object.Filter := Filter;
               loop
                  Object.Waiting := True;
                  Wait (
                     Object.Condition_Variable,
                     Object.Mutex,
                     Timeout => Abort_Checking_Span,
                     Notified => Notified,
                     Aborted => Aborted);
                  Object.Waiting := False;
                  exit Taking when Aborted;
                  exit when Object.Tail /= Tail_On_Waiting;
               end loop;
               if Tail_On_Waiting /= null then
                  Previous := Tail_On_Waiting;
                  I := Tail_On_Waiting.Next;
               else
                  Previous := null;
                  I := Object.Head;
               end if;
            end Not_Found;
         end loop Taking;
      end;
      Leave (Object.Mutex);
   end Take;

   function Count (
      Object : not null access Queue;
      Params : Address;
      Filter : Queue_Filter)
      return Natural
   is
      Result : Natural := 0;
   begin
      Enter (Object.Mutex);
      declare
         I : Queue_Node_Access := Object.Head;
      begin
         while I /= null loop
            if Filter = null or else Filter (I, Params) then
               Result := Result + 1;
            end if;
            I := I.Next;
         end loop;
      end;
      Leave (Object.Mutex);
      return Result;
   end Count;

   function Canceled (Object : Queue) return Boolean is
   begin
      return Object.Canceled;
   end Canceled;

   --  event

   procedure Finalize (Object : in out Event) is
   begin
      Finalize (Object.Mutex);
      Finalize (Object.Condition_Variable);
   end Finalize;

   procedure Set (Object : in out Event) is
   begin
      Enter (Object.Mutex);
      Object.Value := True;
      Notify_All (Object.Condition_Variable);
      Leave (Object.Mutex);
   end Set;

   procedure Reset (Object : in out Event) is
   begin
      Enter (Object.Mutex);
      Object.Value := False;
      Leave (Object.Mutex);
   end Reset;

   function Get (Object : Event) return Boolean is
   begin
      return Object.Value; -- atomic, is it ok?
   end Get;

   procedure Wait (Object : in out Event) is
   begin
      Enter (Object.Mutex);
      if not Object.Value then
         loop
            Wait (
               Object.Condition_Variable,
               Object.Mutex);
            exit when Object.Value;
         end loop;
      end if;
      Leave (Object.Mutex);
   end Wait;

   procedure Wait (
      Object : in out Event;
      Aborted : out Boolean) is
   begin
      Enter (Object.Mutex);
      if Object.Value then
         Aborted := Is_Aborted;
      else
         loop
            Wait (
               Object.Condition_Variable,
               Object.Mutex,
               Aborted => Aborted);
            exit when Object.Value or else Aborted;
         end loop;
      end if;
      Leave (Object.Mutex);
   end Wait;

   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean;
      Aborted : out Boolean) is
   begin
      Enter (Object.Mutex);
      if Object.Value then
         Value := True;
         Aborted := Is_Aborted;
      else
         Wait (
            Object.Condition_Variable,
            Object.Mutex,
            Timeout => Timeout,
            Notified => Value,
            Aborted => Aborted);
         pragma Assert (Object.Value >= Value);
      end if;
      Leave (Object.Mutex);
   end Wait;

   --  barrier

   procedure Initialize (
      Object : in out Barrier;
      Release_Threshold : Natural) is
   begin
      Object.Release_Threshold := Release_Threshold;
      Object.Blocked := 0;
   end Initialize;

   procedure Finalize (Object : in out Barrier) is
   begin
      Finalize (Object.Mutex);
      Finalize (Object.Condition_Variable);
   end Finalize;

   procedure Wait (
      Object : in out Barrier;
      Notified : out Boolean;
      Aborted : out Boolean) is
   begin
      Enter (Object.Mutex);
      Notified := Object.Blocked = 0;
      Object.Blocked := Object.Blocked + 1;
      if Object.Blocked = Object.Release_Threshold then
         Notify_All (Object.Condition_Variable);
         Object.Blocked := 0;
         Aborted := Is_Aborted;
      else
         Wait (
            Object.Condition_Variable,
            Object.Mutex,
            Aborted => Aborted);
      end if;
      Leave (Object.Mutex);
   end Wait;

   --  multi-read/exclusive-write lock

   procedure Finalize (Object : in out RW_Lock) is
   begin
      if C.pthread.pthread_rwlock_destroy (Object.Handle'Access) /= 0 then
         null; -- raise Tasking_Error;
      end if;
   end Finalize;

   procedure Enter_Reading (Object : in out RW_Lock) is
   begin
      if C.pthread.pthread_rwlock_rdlock (Object.Handle'Access) /= 0 then
         raise Tasking_Error;
      end if;
   end Enter_Reading;

   procedure Leave_Reading (Object : in out RW_Lock) is
   begin
      if C.pthread.pthread_rwlock_unlock (Object.Handle'Access) /= 0 then
         raise Tasking_Error;
      end if;
   end Leave_Reading;

   procedure Enter_Writing (Object : in out RW_Lock) is
   begin
      if C.pthread.pthread_rwlock_wrlock (Object.Handle'Access) /= 0 then
         raise Tasking_Error;
      end if;
   end Enter_Writing;

end System.Tasking.Inside;
