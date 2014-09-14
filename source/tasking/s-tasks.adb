pragma Check_Policy (Trace, Off);
with Ada.Exception_Identification.From_Here;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Formatting.Address;
with System.Native_Stack;
with System.Native_Tasks.Yield;
with System.Native_Time;
with System.Runtime_Context;
with System.Shared_Locking;
with System.Standard_Allocators;
with System.Synchronous_Control;
with System.Synchronous_Objects.Abortable;
with System.Termination;
with System.Unbounded_Stack_Allocators;
with System.Unwind.Raising;
package body System.Tasks is
   use Ada.Exception_Identification.From_Here;
   use type Synchronous_Objects.Queue_Node_Access;
   use type Storage_Elements.Storage_Offset;

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

   --  delay statement

   procedure Delay_For (D : Duration);
   procedure Delay_For (D : Duration) is
      Aborted : Boolean;
   begin
      Enable_Abort;
      Synchronous_Objects.Abortable.Delay_For (
         D,
         Aborted => Aborted);
      Disable_Abort (Aborted);
   end Delay_For;

   procedure Delay_Until (T : Native_Time.Native_Time);
   procedure Delay_Until (T : Native_Time.Native_Time) is
      Aborted : Boolean;
   begin
      Enable_Abort;
      Synchronous_Objects.Abortable.Delay_Until (
         T,
         Aborted => Aborted);
      Disable_Abort (Aborted);
   end Delay_Until;

   --  shared lock

   Shared_Lock : Synchronous_Objects.Mutex; -- uninitialized

   procedure Shared_Lock_Enter;
   procedure Shared_Lock_Enter is
   begin
      Synchronous_Objects.Enter (Shared_Lock);
   end Shared_Lock_Enter;

   procedure Shared_Lock_Leave;
   procedure Shared_Lock_Leave is
   begin
      Synchronous_Objects.Leave (Shared_Lock);
   end Shared_Lock_Leave;

   --  attributes

   generic
      type Element_Type is private;
      type Array_Type is array (Natural) of Element_Type;
      type Array_Access is access all Array_Type;
   package Simple_Vectors is

      procedure Expand (
         Data : in out Array_Access;
         Length : in out Natural;
         New_Length : Natural;
         New_Item : Element_Type);

      procedure Clear (Data : in out Array_Access; Length : in out Natural);

   end Simple_Vectors;

   package body Simple_Vectors is

      package AA_Conv is
         new Address_To_Named_Access_Conversions (Array_Type, Array_Access);

      procedure Expand (
         Data : in out Array_Access;
         Length : in out Natural;
         New_Length : Natural;
         New_Item : Element_Type) is
      begin
         if New_Length > Length then
            Data := AA_Conv.To_Pointer (
               Standard_Allocators.Reallocate (
                  AA_Conv.To_Address (Data),
                  Storage_Elements.Storage_Count (New_Length)
                     * (Array_Type'Component_Size / Standard'Storage_Unit)));
            for I in Length .. New_Length - 1 loop
               Data (I) := New_Item;
            end loop;
            Length := New_Length;
         end if;
      end Expand;

      procedure Clear (Data : in out Array_Access; Length : in out Natural) is
      begin
         Standard_Allocators.Free (AA_Conv.To_Address (Data));
         Data := null;
         Length := 0;
      end Clear;

   end Simple_Vectors;

   package Attribute_Vectors is
      new Simple_Vectors (Attribute, Attribute_Array, Attribute_Array_Access);

   procedure Clear_Attributes (Item : Task_Id);
   procedure Clear_Attributes (Item : Task_Id) is
   begin
      for I in 0 .. Item.Attributes_Length - 1 loop
         declare
            A : Attribute
               renames Item.Attributes (I);
         begin
            if A.Index /= null then
               Clear (Item, A.Index.all);
            end if;
         end;
      end loop;
      Attribute_Vectors.Clear (Item.Attributes, Item.Attributes_Length);
   end Clear_Attributes;

   Attribute_Indexes_Lock : Synchronous_Objects.Mutex; -- uninitialized

   type Attribute_Index_Set is array (Natural) of Word;
   pragma Suppress_Initialization (Attribute_Index_Set);
   type Attribute_Index_Set_Access is access all Attribute_Index_Set;

   Attribute_Indexes : Attribute_Index_Set_Access := null;
   Attribute_Indexes_Length : Natural := 0;

   package Attribute_Index_Sets is
      new Simple_Vectors (
         Word,
         Attribute_Index_Set,
         Attribute_Index_Set_Access);

   --  task record

   package Task_Record_Conv is
      new Address_To_Named_Access_Conversions (Task_Record, Task_Id);

   procedure Append_To_Completion_List (
      Master : Master_Access;
      Item : Task_Id);
   procedure Append_To_Completion_List (
      Master : Master_Access;
      Item : Task_Id) is
   begin
      Synchronous_Objects.Enter (Master.Mutex);
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
      Synchronous_Objects.Leave (Master.Mutex);
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
            Mutex_Ref : constant not null access Synchronous_Objects.Mutex :=
               Item.Master_Of_Parent.Mutex'Access;
         begin
            Synchronous_Objects.Enter (Mutex_Ref.all);
            Remove_From_Completion_List_No_Sync (Item);
            Synchronous_Objects.Leave (Mutex_Ref.all);
         end;
      end if;
   end Remove_From_Completion_List;

   procedure Free (Item : in out Task_Id);
   procedure Free (Item : in out Task_Id) is
      procedure Unchecked_Free is
         new Ada.Unchecked_Deallocation (String, String_Access);
      procedure Unchecked_Free is
         new Ada.Unchecked_Deallocation (Rendezvous_Record, Rendezvous_Access);
      procedure Unchecked_Free is
         new Ada.Unchecked_Deallocation (Task_Record, Task_Id);
   begin
      --  detach from master
      Remove_From_Completion_List (Item);
      --  finalize abort
      Native_Tasks.Finalize (Item.Abort_Attribute);
      --  free attributes
      Clear_Attributes (Item);
      --  free task record
      if Item.Rendezvous /= null then
         Synchronous_Objects.Finalize (Item.Rendezvous.Calling);
         Synchronous_Objects.Finalize (Item.Rendezvous.Mutex);
         Unchecked_Free (Item.Rendezvous);
      end if;
      Unchecked_Free (Item.Name);
      Unchecked_Free (Item);
   end Free;

   --  thead id

   TLS_Current_Task_Id : Task_Id := null;
   pragma Thread_Local_Storage (TLS_Current_Task_Id);

   Main_Task_Record : aliased Task_Record (Main);

   --  task local storage (secondary stack and exception occurrence)

   TLS_Data : Runtime_Context.Task_Local_Storage_Access := null;
   pragma Thread_Local_Storage (TLS_Data);

   function Get_TLS
      return not null Runtime_Context.Task_Local_Storage_Access;
   function Get_TLS
      return not null Runtime_Context.Task_Local_Storage_Access is
   begin
      return TLS_Data;
   end Get_TLS;

   --  name

   Main_Name : aliased constant String := "*main";
   Null_Name : aliased constant String := "";

   --  signal handler

   procedure Abort_Signal_Handler;
   procedure Abort_Signal_Handler is
      T : constant Task_Id := TLS_Current_Task_Id;
      Error : Boolean;
   begin
      pragma Check (Trace, Ada.Debug.Put (Name (T).all));
      T.Aborted := True;
      if T.Abort_Handler /= null then
         T.Abort_Handler (T);
      end if;
      if T.Kind = Main then
         Native_Tasks.Uninstall_Abort_Handler;
         Native_Tasks.Send_Abort_Signal (
            T.Handle,
            T.Abort_Attribute,
            Error); -- ignore error
      end if;
   end Abort_Signal_Handler;

   --  registration

   type Registered_State_Type is (Single_Task, Registered, Unregistered);
   pragma Discard_Names (Registered_State_Type);
   Registered_State : Registered_State_Type := Single_Task;
   pragma Atomic (Registered_State);

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
      pragma Assert (Main_Task_Record.Abort_Locking = 2);
      TLS_Current_Task_Id := null;
      --  shared lock
      Synchronous_Objects.Finalize (Shared_Lock);
      Shared_Locking.Enter_Hook := Shared_Locking.Nop'Access;
      Shared_Locking.Leave_Hook := Shared_Locking.Nop'Access;
      --  yield
      Synchronous_Control.Yield_Hook := Synchronous_Control.Nop'Access;
      --  delay statement
      Native_Time.Delay_For_Hook := Native_Time.Simple_Delay_For'Access;
      Native_Time.Delay_Until_Hook := Native_Time.Simple_Delay_Until'Access;
      --  attribute indexes
      Synchronous_Objects.Finalize (Attribute_Indexes_Lock);
      Attribute_Index_Sets.Clear (Attribute_Indexes, Attribute_Indexes_Length);
      --  task local storage (secondary stack and exception occurrence)
      Runtime_Context.Get_Task_Local_Storage_Hook :=
         Runtime_Context.Get_Main_Task_Local_Storage'Access;
      --  main thread id
      Native_Tasks.Finalize (Main_Task_Record.Abort_Attribute);
      --  signal handler
      Native_Tasks.Uninstall_Abort_Handler;
      --  clear
      Registered_State := Unregistered;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Unregister;

   procedure Register;
   procedure Register is
   begin
      if Registered_State /= Registered then
         pragma Check (Trace, Ada.Debug.Put ("enter"));
         --  still it is single thread
         Registered_State := Registered;
         Termination.Register_Exit (Unregister'Access);
         --  shared lock
         Synchronous_Objects.Initialize (Shared_Lock);
         Shared_Locking.Enter_Hook := Shared_Lock_Enter'Access;
         Shared_Locking.Leave_Hook := Shared_Lock_Leave'Access;
         --  yield
         Synchronous_Control.Yield_Hook := Native_Tasks.Yield'Access;
         --  delay statement
         Native_Time.Delay_For_Hook := Delay_For'Access;
         Native_Time.Delay_Until_Hook := Delay_Until'Access;
         --  attribute indexes
         Synchronous_Objects.Initialize (Attribute_Indexes_Lock);
         --  task local storage (secondary stack and exception occurrence)
         TLS_Data := Runtime_Context.Get_Main_Task_Local_Storage;
         Runtime_Context.Get_Task_Local_Storage_Hook := Get_TLS'Access;
         --  main thread id
         TLS_Current_Task_Id := Main_Task_Record'Access;
         Main_Task_Record.Handle := Native_Tasks.Current;
         Main_Task_Record.Aborted := False;
         Main_Task_Record.Abort_Handler := null;
         Main_Task_Record.Abort_Locking := 2;
         Main_Task_Record.Attributes := null;
         Main_Task_Record.Attributes_Length := 0;
         Main_Task_Record.Activation_State := AS_Active;
         Main_Task_Record.Termination_State := TS_Active;
         Main_Task_Record.Master_Level := Environment_Task_Level;
         Main_Task_Record.Master_Top := null; -- start from Library_Task_Level
         Native_Tasks.Initialize (Main_Task_Record.Abort_Attribute);
         --  signal handler
         Native_Tasks.Install_Abort_Handler (Abort_Signal_Handler'Access);
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
      function Cast is
         new Ada.Unchecked_Conversion (
            Ada.Exceptions.Exception_Occurrence,
            Unwind.Exception_Occurrence);
      Name_Prefix : constant String := "task ";
      Name : String (
         1 ..
         Name_Prefix'Length
            + (if T.Name /= null then T.Name'Length + 1 else 0)
            + Formatting.Address.Address_String'Length);
      Name_Last : Natural;
   begin
      Name_Last := Name_Prefix'Length;
      Name (1 .. Name_Last) := Name_Prefix;
      if T.Name /= null then
         Name (Name_Last + 1 .. Name_Last + T.Name'Length) := T.Name.all;
         Name_Last := Name_Last + T.Name'Length + 1;
         Name (Name_Last) := ':';
      end if;
      Formatting.Address.Image (
         Task_Record_Conv.To_Address (T),
         Name (
            Name_Last + 1 ..
            Name_Last + Formatting.Address.Address_String'Length),
         Set => Formatting.Upper_Case);
      Name_Last := Name_Last + Formatting.Address.Address_String'Length;
      Unwind.Raising.Report (Cast (Current), Name (1 .. Name_Last));
   end Report;

   --  Native_Tasks.Result_Type is void * in POSIX, or DWORD in Windows.
   --  Long_Integer is register size in POSIX, and 32bit in Windows.
   TR_Freed : constant := 0;
   TR_Not_Freed : constant := 1;

   function Thread (Rec : Native_Tasks.Parameter_Type)
      return Native_Tasks.Result_Type;
   pragma Convention (Thread_Body_CC, Thread);
   function Thread (Rec : Native_Tasks.Parameter_Type)
      return Native_Tasks.Result_Type
   is
      function To_Address is
         new Ada.Unchecked_Conversion (Native_Tasks.Parameter_Type, Address);
      function To_Result is
         new Ada.Unchecked_Conversion (Long_Integer, Native_Tasks.Result_Type);
      Result : Native_Tasks.Result_Type;
      Local : aliased Runtime_Context.Task_Local_Storage;
      T : Task_Id := Task_Record_Conv.To_Pointer (To_Address (Rec));
      No_Detached : Boolean;
   begin
      TLS_Current_Task_Id := T;
      --  block abort signal
      Native_Tasks.Initialize (T.Abort_Attribute);
      Native_Tasks.Block_Abort_Signal (T.Abort_Attribute);
      T.Abort_Locking := 1;
      --  setup secondary stack
      Local.Secondary_Stack := Null_Address;
      Local.Overlaid_Allocation := Null_Address;
      Local.Current_Exception.Id := null;
      TLS_Data := Local'Unchecked_Access;
      --  setup signal stack
      Unwind.Mapping.Set_Signal_Stack (T.Signal_Stack'Access);
      --  setup native stack
      Native_Tasks.Initialize (T.Stack_Attribute);
      --  execute
      declare
         procedure On_Exception;
         procedure On_Exception is
            Aborted : Boolean; -- ignored
         begin
            pragma Check (Trace, Ada.Debug.Put ("enter"));
            if T.Activation_State < AS_Active then
               pragma Check (Trace, Ada.Debug.Put ("unactivated"));
               pragma Assert (T.Abort_Locking = 1);
               --  an exception was raised until calling Accept_Activation
               T.Activation_Chain.Error := Any_Exception;
               Accept_Activation (Aborted => Aborted);
            end if;
            pragma Check (Trace, Ada.Debug.Put ("leave"));
         end On_Exception;
      begin
         if T.Activation_Chain /= null then
            --  Abort_Undefer will be called on activation
            T.Abort_Locking := T.Abort_Locking + 1;
         end if;
         T.Process (T.Params);
      exception
         when Standard'Abort_Signal =>
            pragma Check (Trace, Ada.Debug.Put ("Abort_Signal"));
            --  Abort_Undefer will not be called by compiler
            if not ZCX_By_Default then
               T.Abort_Locking := T.Abort_Locking - 1;
            end if;
            On_Exception;
         when E : others =>
            Report (T, E);
            On_Exception;
      end;
      pragma Assert (T.Abort_Locking = 1);
      --  cancel calling queue
      Cancel_Calls;
      --  deactivate and set 'Terminated to False
      No_Detached := sync_bool_compare_and_swap (
         T.Termination_State'Access,
         TS_Active,
         TS_Terminated);
      --  cleanup native stack info
      Native_Tasks.Finalize (T.Stack_Attribute);
      --  free
      if No_Detached then
         Result := To_Result (TR_Not_Freed); -- caller or master may wait
      else -- detached
         if T.Master_Of_Parent /= null then
            declare
               Mutex_Ref : constant
                  not null access Synchronous_Objects.Mutex :=
                  T.Master_Of_Parent.Mutex'Access;
            begin
               Synchronous_Objects.Enter (Mutex_Ref.all);
               if T.Auto_Detach then
                  Remove_From_Completion_List_No_Sync (T);
                  declare
                     Error : Boolean;
                  begin
                     Native_Tasks.Detach (T.Handle, Error);
                     --  error should be reported?
                  end;
               end if;
               Synchronous_Objects.Leave (Mutex_Ref.all);
            end;
            if not T.Auto_Detach then
               --  master already has been waiting
               Result := To_Result (TR_Not_Freed);
            else
               Free (T);
               Result := To_Result (TR_Freed);
            end if;
         else
            Free (T);
            Result := To_Result (TR_Freed);
         end if;
      end if;
      --  cleanup secondary stack
      Unbounded_Stack_Allocators.Clear (Local.Secondary_Stack);
      --  return
      return Result;
   end Thread;

   type Execution_Error is (None, Aborted, Elaboration_Error, Done);
   pragma Discard_Names (Execution_Error);

   procedure Execute (T : Task_Id; Error : out Execution_Error);
   procedure Execute (T : Task_Id; Error : out Execution_Error) is
      function To_Parameter is
         new Ada.Unchecked_Conversion (Address, Native_Tasks.Parameter_Type);
      Creation_Error : Boolean;
   begin
      if not sync_bool_compare_and_swap (
         T.Activation_State'Access,
         AS_Suspended,
         AS_Created)
      then
         Error := Done;
      elsif T.Aborted then -- aborted before activation
         Error := Aborted;
         T.Activation_State := AS_Error;
         T.Termination_State := TS_Terminated; -- C9A004A
      else
         Native_Tasks.Create (
            T.Handle,
            To_Parameter (Task_Record_Conv.To_Address (T)),
            Thread'Access,
            Error => Creation_Error);
         if Creation_Error then
            Error := Elaboration_Error;
            T.Activation_State := AS_Error;
         else
            Error := None;
         end if;
      end if;
   end Execute;

   procedure Wait (T : Task_Id; Free_Task_Id : out Task_Id);
   procedure Wait (T : Task_Id; Free_Task_Id : out Task_Id) is
      function To_Long_Integer is
         new Ada.Unchecked_Conversion (Native_Tasks.Result_Type, Long_Integer);
      Rec : aliased Native_Tasks.Result_Type;
      Error : Boolean;
   begin
      if T.Activation_State = AS_Error then
         Free_Task_Id := T;
      else
         Native_Tasks.Join (
            T.Handle,
            Abort_Attribute,
            Rec,
            Error);
         if Error then
            Raise_Exception (Tasking_Error'Identity);
         end if;
         if To_Long_Integer (Rec) = TR_Freed then
            Free_Task_Id := null;
         else -- Rec = TR_Not_Freed
            Free_Task_Id := T;
         end if;
      end if;
   end Wait;

   --  abort

   procedure Set_Abort_Recursively (T : Task_Id);
   procedure Set_Abort_Recursively (T : Task_Id) is
   begin
      T.Aborted := True;
      declare
         M : Master_Access := T.Master_Top;
      begin
         while M /= null loop
            Synchronous_Objects.Enter (M.Mutex);
            declare
               L : Task_Id := M.List;
            begin
               if L /= null then
                  loop
                     Set_Abort_Recursively (L);
                     L := L.Next_At_Same_Level;
                     exit when L = M.List;
                  end loop;
               end if;
            end;
            Synchronous_Objects.Leave (M.Mutex);
            M := M.Previous;
         end loop;
      end;
   end Set_Abort_Recursively;

   procedure Abort_Handler_On_Leave_Master (T : Task_Id);
   procedure Abort_Handler_On_Leave_Master (T : Task_Id) is
      M : constant Master_Access := T.Master_Top;
      Error : Boolean;
      Top_Child : Task_Id;
   begin
      --  Enter (M.Mutex);
      Top_Child := M.List;
      Native_Tasks.Send_Abort_Signal (
         Top_Child.Handle,
         Top_Child.Abort_Attribute,
         Error);
      if Error then
         Raise_Exception (Tasking_Error'Identity);
      end if;
      --  Leave (M.Mutex);
   end Abort_Handler_On_Leave_Master;

   --  activation

   procedure Remove_From_Merged_Activation_Chain_List (
      C : not null Activation_Chain);
   procedure Remove_From_Merged_Activation_Chain_List (
      C : not null Activation_Chain)
   is
      pragma Suppress (Accessibility_Check);
      Chain : constant access Activation_Chain := C.Self;
   begin
      if Chain.all = C then
         Chain.all := Chain.all.Merged;
      else
         declare
            I : Activation_Chain := Chain.all;
         begin
            while I.Merged /= C loop
               I := I.Merged;
            end loop;
            I.Merged := C.Merged;
         end;
      end if;
   end Remove_From_Merged_Activation_Chain_List;

   procedure Release (C : in out Activation_Chain);
   procedure Release (C : in out Activation_Chain) is
      procedure Free (Item : in out Activation_Chain);
      procedure Free (Item : in out Activation_Chain) is
         procedure Unchecked_Free is
            new Ada.Unchecked_Deallocation (
               Activation_Chain_Data,
               Activation_Chain);
      begin
         Synchronous_Objects.Finalize (Item.Mutex);
         Synchronous_Objects.Finalize (Item.Condition_Variable);
         Unchecked_Free (Item);
      end Free;
   begin
      if sync_add_and_fetch (C.Release_Count'Access, 1) > C.Task_Count then
         Free (C);
      end if;
   end Release;

   procedure Set_Active (T : not null Task_Id; State : Activation_State);
   procedure Set_Active (T : not null Task_Id; State : Activation_State) is
   begin
      if not Elaborated (T) then
         pragma Check (Trance, Ada.Debug.Put (
            "elab error in " & Name (T).all));
         T.Activation_Chain.Error := Elaboration_Error;
      end if;
      T.Activation_State := State;
   end Set_Active;

   procedure Set_Active (C : not null Activation_Chain);
   procedure Set_Active (C : not null Activation_Chain) is
      I : Task_Id := C.List;
   begin
      while I /= null loop
         if I.Activation_State < AS_Active then
            Set_Active (I, AS_Active);
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
      C : Activation_Chain := Chain.all;
   begin
      Error := None;
      Aborted := False;
      while C /= null loop
         pragma Assert (C.Self = Chain);
         Synchronous_Objects.Enter (C.Mutex);
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
            Synchronous_Objects.Notify_All (C.Condition_Variable);
         else
            loop
               Synchronous_Objects.Wait (C.Condition_Variable, C.Mutex);
               Aborted := Is_Aborted; -- is this check worthwhile?
               exit when C.Activated_Count > C.Task_Count or else Aborted;
            end loop;
         end if;
         Error := Activation_Error'Max (Error, C.Error);
         Remove_From_Merged_Activation_Chain_List (C);
         Synchronous_Objects.Leave (C.Mutex);
         --  cleanup
         declare
            Merged : constant Activation_Chain := C.Merged;
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
            C : constant Activation_Chain := T.Activation_Chain;
         begin
            Synchronous_Objects.Enter (C.Mutex);
            if Error_On_Execute /= None then
               C.Task_Count := C.Task_Count - 1;
               if Error_On_Execute = Elaboration_Error then
                  Error := Elaboration_Error;
               end if;
            else
               Set_Active (T, AS_Active_Before_Activation);
               Synchronous_Objects.Notify_All (C.Condition_Variable);
            end if;
            Error := Activation_Error'Max (Error, C.Error);
            Synchronous_Objects.Leave (C.Mutex);
         end;
      end if;
      --  note, this procedure does not free activation chain
      --  it must call above Activate taking chain
   end Activate;

   --  completion

   procedure Free (Item : in out Master_Access);
   procedure Free (Item : in out Master_Access) is
      procedure Unchecked_Free is
         new Ada.Unchecked_Deallocation (Master_Record, Master_Access);
   begin
      Synchronous_Objects.Finalize (Item.Mutex);
      Unchecked_Free (Item);
   end Free;

   --  queue

   package Queue_Node_Conv is
      new Address_To_Named_Access_Conversions (
         Synchronous_Objects.Queue_Node,
         Synchronous_Objects.Queue_Node_Access);

   function Uncall_Filter (
      The_Node : not null Synchronous_Objects.Queue_Node_Access;
      Params : Address)
      return Boolean;
   function Uncall_Filter (
      The_Node : not null Synchronous_Objects.Queue_Node_Access;
      Params : Address)
      return Boolean is
   begin
      return The_Node = Queue_Node_Conv.To_Pointer (Params);
   end Uncall_Filter;

   --  implementation

   procedure When_Abort_Signal is
   begin
      if not ZCX_By_Default then
         Unlock_Abort;
      end if;
   end When_Abort_Signal;

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
      function To_Process_Handler is
         new Ada.Unchecked_Conversion (Address, Process_Handler);
      Name_Data : String_Access := null;
      Chain_Data : Activation_Chain := null;
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
         Chain_Data := Chain.all;
         if Chain_Data = null then
            pragma Check (Trace, Ada.Debug.Put ("new chain"));
            Chain_Data := new Activation_Chain_Data'(
               List => null,
               Task_Count => 0,
               Activated_Count => 0,
               Release_Count => 0,
               Mutex => <>, -- uninitialized
               Condition_Variable => <>, -- uninitialized
               Error => None,
               Merged => null,
               Self => Chain);
            Synchronous_Objects.Initialize (Chain_Data.Mutex);
            Synchronous_Objects.Initialize (Chain_Data.Condition_Variable);
            Chain.all := Chain_Data;
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
            Mutex => <>, -- uninitialized
            Calling => <>); -- uninitialized
         Synchronous_Objects.Initialize (Rendezvous.Mutex);
         Synchronous_Objects.Initialize (
            Rendezvous.Calling,
            Rendezvous.Mutex'Access);
      end if;
      --  task record
      T := new Task_Record'(
         Kind => Sub,
         Handle => <>, -- uninitialized
         Aborted => False,
         Abort_Handler => null,
         Abort_Locking => 0,
         Attributes => null,
         Attributes_Length => 0,
         Activation_State => AS_Suspended, -- unexecuted
         Termination_State => TS_Active,
         Master_Level => Level,
         Master_Top => null,
         Params => Params,
         Process => To_Process_Handler (Process.all'Address),
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
         Stack_Attribute => <>, -- uninitialized
         Abort_Attribute => <>, -- uninitialized
         Signal_Stack => <>); -- uninitialized
      --  for master
      if Master /= null then
         --  append to the parent's master
         Append_To_Completion_List (Master, T);
      end if;
      --  for activation
      if Chain_Data /= null then
         --  apeend to activation chain
         pragma Check (Trace, Ada.Debug.Put ("append to the chain"));
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
            Raise_Exception (Tasking_Error'Identity);
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
               Synchronous_Objects.Enter (T.Master_Of_Parent.Mutex);
               T.Auto_Detach := True;
               Synchronous_Objects.Leave (T.Master_Of_Parent.Mutex);
            else
               declare
                  Orig_T : constant Task_Id := T;
                  Error : Boolean;
               begin
                  T := null;
                  Native_Tasks.Detach (Orig_T.Handle, Error);
                  if Error then
                     Raise_Exception (Tasking_Error'Identity);
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
      elsif Registered_State = Unregistered then -- main has been terminated
         return True;
      else
         return T.Termination_State = TS_Terminated;
      end if;
   end Terminated;

   function Activated (T : Task_Id) return Boolean is
   begin
      if T = null then
         raise Program_Error; -- RM C.7.1(15)
      elsif Registered_State = Unregistered then -- main has been terminated
         return True;
      else
         return T.Activation_State in AS_Active_Before_Activation .. AS_Active;
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
      Size : out Storage_Elements.Storage_Count)
   is
      Top, Bottom : Address;
   begin
      Native_Stack.Get (
         Native_Tasks.Info_Block (T.Handle, T.Stack_Attribute),
         Top,
         Bottom);
      Addr := Top;
      Size := Bottom - Top;
   end Get_Stack;

   function Name (T : not null Task_Id)
      return not null access constant String is
   begin
      if T.Kind = Main then
         return Main_Name'Access;
      elsif T.Name = null then
         return Null_Name'Access;
      else
         return T.Name;
      end if;
   end Name;

   procedure Send_Abort (T : not null Task_Id) is
      Current_Task_Id : constant Task_Id := TLS_Current_Task_Id;
      Error : Boolean;
   begin
      pragma Check (Trace, Ada.Debug.Put (
         Name (Current_Task_Id).all & " aborts " & Name (T).all));
      if T = Current_Task_Id then
         Set_Abort_Recursively (T);
         raise Standard'Abort_Signal;
      elsif T.Activation_State = AS_Suspended then
         T.Aborted := True;
      else
         Native_Tasks.Send_Abort_Signal (T.Handle, T.Abort_Attribute, Error);
         if Error then
            Raise_Exception (Tasking_Error'Identity);
         end if;
         Set_Abort_Recursively (T); -- set 'Callable to false, C9A009H
         --  abort myself if parent task is aborted, C9A007A
         declare
            P : Task_Id := Current_Task_Id;
         begin
            loop
               P := Parent (P);
               exit when P = null;
               if P = T then
                  pragma Assert (Current_Task_Id.Aborted);
                  raise Standard'Abort_Signal;
               end if;
            end loop;
         end;
      end if;
   end Send_Abort;

   procedure Enable_Abort is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      if T /= null then
         pragma Check (Trace, Ada.Debug.Put (Name (T).all
            & Natural'Image (T.Abort_Locking) & " =>"
            & Natural'Image (T.Abort_Locking - 1)));
         pragma Assert (T.Abort_Locking > 0);
         T.Abort_Locking := T.Abort_Locking - 1;
         if T.Kind = Sub then
            pragma Assert (T.Abort_Locking = 0);
            Native_Tasks.Unblock_Abort_Signal (T.Abort_Attribute);
         end if;
      end if;
   end Enable_Abort;

   procedure Disable_Abort (Aborted : Boolean) is
      pragma Unreferenced (Aborted); -- dummy parameter for coding check
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      if T /= null then
         pragma Check (Trace, Ada.Debug.Put (Name (T).all
            & Natural'Image (T.Abort_Locking) & " =>"
            & Natural'Image (T.Abort_Locking + 1)));
         if T.Kind = Sub then
            pragma Assert (T.Abort_Locking = 0);
            Native_Tasks.Block_Abort_Signal (T.Abort_Attribute);
         end if;
         T.Abort_Locking := T.Abort_Locking + 1;
         if T.Aborted and then T.Abort_Locking = 1 then
            raise Standard'Abort_Signal;
         end if;
      end if;
   end Disable_Abort;

   procedure Lock_Abort is
   begin
      if Registered_State = Registered then
         declare
            T : constant Task_Id := Current_Task_Id;
         begin
            pragma Check (Trace, Ada.Debug.Put (Name (T).all
               & Natural'Image (T.Abort_Locking) & " =>"
               & Natural'Image (T.Abort_Locking + 1)));
            T.Abort_Locking := T.Abort_Locking + 1;
         end;
      end if;
   end Lock_Abort;

   procedure Unlock_Abort is
   begin
      if Registered_State = Registered then
         declare
            T : constant Task_Id := TLS_Current_Task_Id;
         begin
            pragma Check (Trace, Ada.Debug.Put (Name (T).all
               & Natural'Image (T.Abort_Locking) & " =>"
               & Natural'Image (T.Abort_Locking - 1)));
            T.Abort_Locking := T.Abort_Locking - 1;
         end;
      end if;
   end Unlock_Abort;

   function Is_Aborted return Boolean is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      return T /= null and then T.Aborted;
   end Is_Aborted;

   function Abort_Attribute
      return access Native_Tasks.Task_Attribute_Of_Abort
   is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      if T /= null and then Registered_State = Registered then
         return T.Abort_Attribute'Access;
      else
         return null;
      end if;
   end Abort_Attribute;

   function Elaborated (T : not null Task_Id) return Boolean is
   begin
      return T.Elaborated = null or else T.Elaborated.all;
   end Elaborated;

   procedure Accept_Activation (Aborted : out Boolean) is
      T : constant Task_Id := TLS_Current_Task_Id;
      C : Activation_Chain := T.Activation_Chain;
   begin
      pragma Assert (C /= null);
      Synchronous_Objects.Enter (C.Mutex);
      Aborted := T.Aborted;
      C.Activated_Count := C.Activated_Count + 1;
      if C.Activated_Count > C.Task_Count then
         Set_Active (C);
         Synchronous_Objects.Notify_All (C.Condition_Variable);
      else
         while T.Activation_State <= AS_Created and then not Aborted loop
            Synchronous_Objects.Wait (C.Condition_Variable, C.Mutex);
            Aborted := T.Aborted; -- is this check worthwhile?
         end loop;
      end if;
      --  elaboration error shold be delivered, but other exceptions should not
      Aborted := Aborted or else C.Error = Elaboration_Error;
      Synchronous_Objects.Leave (C.Mutex);
      --  cleanup
      Release (C);
      pragma Check (Trace,
         Ada.Debug.Put ("aborted = " & Boolean'Image (Aborted)));
   end Accept_Activation;

   procedure Activate (
      Chain : not null access Activation_Chain;
      Aborted : out Boolean)
   is
      Error : Activation_Error;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      Activate (Chain, Error, Aborted => Aborted);
      case Error is
         when None =>
            null;
         when Elaboration_Error =>
            raise Program_Error; -- C39008A, RM 3.11 (14)
         when Any_Exception =>
            Raise_Exception (Tasking_Error'Identity); -- C93004A
      end case;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Activate;

   procedure Activate (T : not null Task_Id) is
      Error : Activation_Error;
   begin
      Activate (T, Error);
      case Error is
         when None =>
            null;
         when Elaboration_Error =>
            raise Program_Error;
         when Any_Exception =>
            Raise_Exception (Tasking_Error'Identity);
      end case;
   end Activate;

   procedure Move (
      From, To : not null access Activation_Chain;
      New_Master : Master_Access) is
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      --  note: keep master level of tasks because it's meaningless
      if From.all /= null then
         --  change completion lists
         declare
            A : Activation_Chain := From.all;
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
         if To.all = null then
            To.all := From.all;
         else
            declare
               I : Activation_Chain := To.all;
            begin
               while I.Merged /= null loop
                  I := I.Merged;
               end loop;
               I.Merged := From.all;
            end;
         end if;
         From.all := null;
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Move;

   function Parent (T : not null Task_Id) return Task_Id is
   begin
      if T.Kind = Main or else T.Master_Of_Parent = null then
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
      pragma Check (Trace, Ada.Debug.Put ("enter in " & Name (T).all));
      declare
         New_Master : constant Master_Access := new Master_Record'(
            Previous => T.Master_Top,
            Parent => T,
            Within => Master_Level'Max (Master_Within, Library_Task_Level) + 1,
            List => null,
            Mutex => <>); -- uninitialized
      begin
         Synchronous_Objects.Initialize (New_Master.Mutex);
         T.Master_Top := New_Master;
      end;
      pragma Check (Trace, Ada.Debug.Put ("leave in " & Name (T).all));
   end Enter_Master;

   procedure Leave_Master is
      T : constant Task_Id := TLS_Current_Task_Id;
      pragma Check (Trace, Ada.Debug.Put ("enter in " & Name (T).all));
      M : Master_Access := T.Master_Top;
      Free_List : Task_Id := null;
   begin
      Synchronous_Objects.Enter (M.Mutex);
      while M.List /= null loop
         declare
            Taken : constant Task_Id := M.List;
            Free_Task_Id : Task_Id;
            A_Error : Activation_Error; -- ignored
            S_Error : Boolean; -- ignored
            Aborted : Boolean; -- ignored
         begin
            Taken.Auto_Detach := False; -- mark inside mutex
            Synchronous_Objects.Leave (M.Mutex);
            if Taken.Activation_State = AS_Suspended then
               pragma Assert (Taken.Activation_Chain /= null);
               --  the task has not been activated
               Taken.Activation_Chain.Error := Elaboration_Error;
               if Taken.Activation_Chain_Living then
                  Activate (
                     Taken.Activation_Chain.Self,
                     Error => A_Error,
                     Aborted => Aborted);
               end if;
            elsif T.Aborted then
               --  C9A007A
               Native_Tasks.Send_Abort_Signal (
                  Taken.Handle,
                  Taken.Abort_Attribute,
                  Error => S_Error);
            end if;
            pragma Assert (T.Abort_Handler = null);
            T.Abort_Handler := Abort_Handler_On_Leave_Master'Access;
            if T.Kind = Sub then
               Native_Tasks.Unblock_Abort_Signal (T.Abort_Attribute);
            end if;
            Wait (Taken, Free_Task_Id);
            if T.Kind = Sub then
               Native_Tasks.Block_Abort_Signal (T.Abort_Attribute);
            end if;
            T.Abort_Handler := null;
            if Free_Task_Id /= null then
               Remove_From_Completion_List (Free_Task_Id);
               Free_Task_Id.Next_At_Same_Level := Free_List;
               Free_List := Free_Task_Id;
            end if;
            Synchronous_Objects.Enter (M.Mutex);
         end;
      end loop;
      Synchronous_Objects.Leave (M.Mutex);
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
      pragma Check (Trace, Ada.Debug.Put ("leave in " & Name (T).all));
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

   procedure Cancel_Calls is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      if T.Rendezvous /= null then
         Synchronous_Objects.Cancel (
            T.Rendezvous.Calling,
            Cancel_Node => Cancel_Call_Hook);
      end if;
   end Cancel_Calls;

   procedure Call (
      T : not null Task_Id;
      Item : not null Synchronous_Objects.Queue_Node_Access) is
   begin
      Synchronous_Objects.Add (T.Rendezvous.Calling, Item);
   end Call;

   procedure Uncall (
      T : not null Task_Id;
      Item : not null Synchronous_Objects.Queue_Node_Access;
      Already_Taken : out Boolean)
   is
      Taken : Synchronous_Objects.Queue_Node_Access;
   begin
      Synchronous_Objects.Take (
         T.Rendezvous.Calling,
         Taken,
         Queue_Node_Conv.To_Address (Item),
         Uncall_Filter'Access);
      Already_Taken := Taken = null;
      pragma Assert (Taken = null or else Taken = Item);
   end Uncall;

   procedure Accept_Call (
      Item : out Synchronous_Objects.Queue_Node_Access;
      Params : Address;
      Filter : Synchronous_Objects.Queue_Filter;
      Aborted : out Boolean)
   is
      T : constant Task_Id := TLS_Current_Task_Id;
   begin
      Synchronous_Objects.Abortable.Take (
         T.Rendezvous.Calling,
         Item,
         Params,
         Filter,
         Aborted);
   end Accept_Call;

   function Call_Count (
      T : not null Task_Id;
      Params : Address;
      Filter : Synchronous_Objects.Queue_Filter)
      return Natural is
   begin
      if not Callable (T) or else T.Rendezvous = null then
         return 0;
      else
         return Synchronous_Objects.Count (
            T.Rendezvous.Calling,
            Params,
            Filter);
      end if;
   end Call_Count;

   function Callable (T : not null Task_Id) return Boolean is
   begin
      return not Terminated (T)
         and then T.Kind = Sub
         and then not T.Aborted
         and then (
            T.Rendezvous = null
            or else not Synchronous_Objects.Canceled (T.Rendezvous.Calling));
   end Callable;

   --  attribute

   procedure Allocate (Index : in out Attribute_Index) is
   begin
      Register; -- Ada.Task_Attributes can be instantiated before tasks
      Synchronous_Objects.Enter (Attribute_Indexes_Lock);
      --  initialization because Suppress_Initialization
      Index.List := null;
      Synchronous_Objects.Initialize (Index.Mutex);
      --  search unused index
      for I in 0 .. Attribute_Indexes_Length - 1 loop
         if Attribute_Indexes (I) /= not 0 then
            for B in Integer range 0 .. Word'Size - 1 loop
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
      Synchronous_Objects.Leave (Attribute_Indexes_Lock);
   end Allocate;

   procedure Free (Index : in out Attribute_Index) is
   begin
      if Registered_State /= Unregistered then
         Synchronous_Objects.Enter (Attribute_Indexes_Lock);
      end if;
      while Index.List /= null loop
         declare
            Next : constant Task_Id :=
               Index.List.Attributes (Index.Index).Next;
         begin
            declare
               A : Attribute
                  renames Index.List.Attributes (Index.Index);
            begin
               A.Finalize (A.Item);
               A.Index := null;
            end;
            Index.List := Next;
         end;
      end loop;
      if Registered_State /= Unregistered then
         declare
            P : constant Natural := Index.Index / Word'Size;
            B : constant Natural := Index.Index mod Word'Size;
         begin
            Attribute_Indexes (P) := Attribute_Indexes (P) and not (2 ** B);
         end;
      end if;
      Synchronous_Objects.Finalize (Index.Mutex);
      if Registered_State /= Unregistered then
         Synchronous_Objects.Leave (Attribute_Indexes_Lock);
      end if;
   end Free;

   procedure Query (
      T : not null Task_Id;
      Index : in out Attribute_Index;
      Process : not null access procedure (Item : Address))
   is
      Value : Address;
   begin
      Synchronous_Objects.Enter (Index.Mutex);
      if T.Attributes_Length > Index.Index
         and then T.Attributes (Index.Index).Index = Index'Unrestricted_Access
      then
         Value := T.Attributes (Index.Index).Item;
      else
         Value := Null_Address;
      end if;
      Process (Value);
      Synchronous_Objects.Leave (Index.Mutex);
   end Query;

   procedure Set (
      T : not null Task_Id;
      Index : in out Attribute_Index;
      New_Item : not null access function return Address;
      Finalize : not null access procedure (Item : Address))
   is
      function To_Finalize_Handler is
         new Ada.Unchecked_Conversion (Address, Finalize_Handler);
   begin
      Synchronous_Objects.Enter (Index.Mutex);
      Attribute_Vectors.Expand (
         T.Attributes,
         T.Attributes_Length,
         Index.Index + 1,
         Attribute'(Index => null, others => <>));
      declare
         A : Attribute
            renames T.Attributes (Index.Index);
      begin
         if A.Index = Index'Unrestricted_Access then
            A.Finalize (A.Item);
         end if;
         A.Item := New_Item.all;
         A.Finalize := To_Finalize_Handler (Finalize.all'Address);
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
      Synchronous_Objects.Leave (Index.Mutex);
   end Set;

   procedure Reference (
      T : not null Task_Id;
      Index : in out Attribute_Index;
      New_Item : not null access function return Address;
      Finalize : not null access procedure (Item : Address);
      Result : out Address)
   is
      function To_Finalize_Handler is
         new Ada.Unchecked_Conversion (Address, Finalize_Handler);
   begin
      Synchronous_Objects.Enter (Index.Mutex);
      Attribute_Vectors.Expand (
         T.Attributes,
         T.Attributes_Length,
         Index.Index + 1,
         Attribute'(Index => null, others => <>));
      declare
         A : Attribute
            renames T.Attributes (Index.Index);
      begin
         if A.Index /= Index'Unrestricted_Access then
            A.Item := New_Item.all;
            A.Finalize := To_Finalize_Handler (Finalize.all'Address);
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
      Synchronous_Objects.Leave (Index.Mutex);
   end Reference;

   procedure Clear (
      T : not null Task_Id;
      Index : in out Attribute_Index) is
   begin
      Synchronous_Objects.Enter (Index.Mutex);
      if T.Attributes_Length > Index.Index
         and then T.Attributes (Index.Index).Index = Index'Unrestricted_Access
      then
         declare
            A : Attribute
               renames T.Attributes (Index.Index);
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
      Synchronous_Objects.Leave (Index.Mutex);
   end Clear;

end System.Tasks;
