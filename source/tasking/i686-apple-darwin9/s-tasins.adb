with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Memory;
with System.Once;
with System.Secondary_Stack;
with System.Shared_Locking;
with System.Storage_Elements;
with System.Tasking.Yield;
with System.Termination;
with System.Unwind;
with C.errno;
with C.sys.time;
with C.sys.types;
package body System.Tasking.Inside is
   pragma Suppress (All_Checks);
   use type C.signed_int;
   use type C.signed_long;
   use type C.void_ptr;

   type Word is mod 2 ** Standard'Word_Size;

   type Time_Rep is range
      -(2 ** (Duration'Size - 1)) ..
      +(2 ** (Duration'Size - 1)) - 1;
   for Time_Rep'Size use Duration'Size;

   function sync_bool_compare_and_swap (
      A1 : not null access Termination_State;
      A2 : Termination_State;
      A3 : Termination_State)
      return Boolean;
   pragma Import (Intrinsic, sync_bool_compare_and_swap,
      "__sync_bool_compare_and_swap_1");

   function sync_sub_and_fetch (
      A1 : not null access Counter;
      A2 : Counter)
      return Counter;
   pragma Import (Intrinsic, sync_sub_and_fetch, "__sync_sub_and_fetch_4");

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
      Item.Master_Of_Parent := Master;
      Item.Next_At_Same_Level := Master.List;
      if Item.Next_At_Same_Level /= null then
         Item.Next_At_Same_Level.Previous_At_Same_Level := Item;
      end if;
      Master.List := Item;
      Leave (Master.Mutex);
   end Append_To_Completion_List;

   procedure Remove_From_Completion_List (Item : Task_Id);
   procedure Remove_From_Completion_List (Item : Task_Id) is
   begin
      if Item.Master_Of_Parent /= null then
         Enter (Item.Master_Of_Parent.Mutex);
         if Item.Previous_At_Same_Level /= null then
            Item.Previous_At_Same_Level.Next_At_Same_Level :=
               Item.Next_At_Same_Level;
         else
            pragma Assert (Item.Master_Of_Parent.List = Item);
            Item.Master_Of_Parent.List := Item.Next_At_Same_Level;
         end if;
         if Item.Next_At_Same_Level /= null then
            Item.Next_At_Same_Level.Previous_At_Same_Level :=
               Item.Previous_At_Same_Level;
         end if;
         Leave (Item.Master_Of_Parent.Mutex);
         Item.Master_Of_Parent := null;
         Item.Previous_At_Same_Level := null;
         Item.Next_At_Same_Level := null;
      end if;
   end Remove_From_Completion_List;

   procedure Free (Item : in out Task_Id);
   procedure Free (Item : in out Task_Id) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (
         Task_Record,
         Task_Id);
   begin
      --  detach from master
      Remove_From_Completion_List (Item);
      --  free attributes
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
      --  free task record
      Unchecked_Free (Item);
   end Free;

   --  activation / completion

   package Activation_Chain_Conv is new Address_To_Named_Access_Conversions (
      Activation_Chain_Data,
      Activation_Chain_Access);

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

   procedure Free (Item : in out Master_Access);
   procedure Free (Item : in out Master_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (
         Master_Data,
         Master_Access);
   begin
      Finalize (Item.Mutex);
      Unchecked_Free (Item);
   end Free;

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

   --  attribute indexes

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

   --  thead id

   TLS_Current_Task_Id : Task_Id := null;
   pragma Thread_Local_Storage (TLS_Current_Task_Id);

   Main_Task_Record : aliased Task_Record (Main);

   --  registration

   Registered : Boolean := False;
   pragma Atomic (Registered);

   procedure Unregister;
   procedure Unregister is
   begin
      --  shared lock
      Finalize (Shared_Lock);
      Shared_Locking.Enter_Hook := Shared_Locking.Nop'Access;
      Shared_Locking.Leave_Hook := Shared_Locking.Nop'Access;
      --  once
      Once.Yield_Hook := Once.Nop'Access;
      --  attribute indexes
      Finalize (Attribute_Indexes_Lock);
      Attribute_Index_Sets.Clear (Attribute_Indexes, Attribute_Indexes_Length);
      --  secondary stack and exception occurrenc
      Soft_Links.Get_Task_Local_Storage :=
         Soft_Links.Get_Main_Task_Local_Storage'Access;
      Soft_Links.Get_Current_Excep := Soft_Links.Get_Main_Current_Excep'Access;
      --  thread id
      null;
   end Unregister;

   procedure Register;
   procedure Register is
   begin
      if not Registered then
         --  still it is single thread
         Registered := True;
         Termination.Register_Exit (Unregister'Access);
         --  shared lock
         Shared_Lock.Handle := C.pthread.PTHREAD_MUTEX_INITIALIZER;
         Shared_Locking.Enter_Hook := Shared_Lock_Enter'Access;
         Shared_Locking.Leave_Hook := Shared_Lock_Leave'Access;
         --  once
         Once.Yield_Hook := Yield'Access;
         --  attribute indexes
         Attribute_Indexes_Lock.Handle := C.pthread.PTHREAD_MUTEX_INITIALIZER;
         --  secondary stack and exception occurrence
         TLS_Stack := Soft_Links.Get_Main_Task_Local_Storage;
         Soft_Links.Get_Task_Local_Storage := Get_SS'Access;
         Soft_Links.Get_Current_Excep := Get_CE'Access;
         --  thread id
         TLS_Current_Task_Id := Main_Task_Record'Access;
         Main_Task_Record.Handle := C.pthread.pthread_self;
         Main_Task_Record.Attributes := null;
         Main_Task_Record.Attributes_Length := 0;
         Main_Task_Record.Activated := True;
         Main_Task_Record.Parent := null;
         Main_Task_Record.Master_Level := Environment_Task_Level;
         Main_Task_Record.Master_Top := null; -- start from Library_Task_Level
      end if;
   end Register;

   --  thread body

   function Thread (Rec : C.void_ptr) return C.void_ptr;
   pragma Convention (C, Thread);
   function Thread (Rec : C.void_ptr) return C.void_ptr is
      Result : C.void_ptr;
      Local : aliased Soft_Links.Task_Local_Storage;
      T : Task_Id := Task_Record_Conv.To_Pointer (To_Address (Rec));
   begin
      TLS_Current_Task_Id := T;
      --  setup secondary stack
      Local.Secondary_Stack := Null_Address;
      TLS_Stack := Local'Unchecked_Access;
      --  activate (auto mode)
      if T.Activation_Chain = null then
         T.Activated := True;
      end if;
      --  execute
      T.Process (T.Params);
      --  deactivate
      if sync_bool_compare_and_swap (
         T.State'Access,
         TS_Active,
         TS_Terminated)
      then
         Result := Rec;
      else
         --  detached
         Free (T);
         Result := C.void_ptr (Null_Address);
      end if;
      --  cleanup secondary stack
      Secondary_Stack.Clear;
      --  return
      return Result;
   end Thread;

   --  implementation

   function Get_Current_Task_Id return Task_Id is
   begin
      Register;
      return TLS_Current_Task_Id;
   end Get_Current_Task_Id;

   function Get_Main_Task_Id return Task_Id is
   begin
      Register;
      return Main_Task_Record'Access;
   end Get_Main_Task_Id;

   procedure Create (
      T : out Task_Id;
      Params : Address;
      Process : not null access procedure (Params : Address);
      Chain : access Activation_Chain := null;
      Master : Master_Level := Soft_Links.Current_Master.all;
      Parent : Task_Id := null)
   is
      Chain_Data : Activation_Chain_Access := null;
      Master_Of_Parent : Master_Access := null;
      Error : C.signed_int;
   begin
      Register;
      pragma Assert (Master /= Foreign_Task_Level + 2);
      --  activation chain
      if Chain /= null then
         Chain_Data := Activation_Chain_Conv.To_Pointer (Chain.Data);
         if Chain_Data = null then
            Chain_Data := new Activation_Chain_Data'(
               List => null,
               Task_Count => 0,
               Activated_Count => 0,
               Mutex => (Handle => C.pthread.PTHREAD_MUTEX_INITIALIZER),
               Condition_Variable =>
                  (Handle => C.pthread.PTHREAD_COND_INITIALIZER),
               Merged => null);
            Chain.Data := Activation_Chain_Conv.To_Address (Chain_Data);
         end if;
      end if;
      --  master
      if Parent /= null then
         pragma Assert (Parent.Master_Top /= null);
         Master_Of_Parent := Parent.Master_Top;
         while Master_Of_Parent.Within > Master loop
            Master_Of_Parent := Master_Of_Parent.Previous;
         end loop;
      end if;
      --  task record
      T := new Task_Record'(
         Kind => Sub,
         Handle => <>,
         Attributes => null,
         Attributes_Length => 0,
         Handle_Received => False,
         Activated => False,
         Parent => Parent,
         Master_Level => Master,
         Master_Top => null,
         Params => Params,
         Process => Process.all'Unrestricted_Access,
         State => TS_Active,
         Activation_Chain => Chain_Data,
         Next_Of_Activation_Chain => null,
         Master_Of_Parent => Master_Of_Parent,
         Previous_At_Same_Level => null,
         Next_At_Same_Level => null);
      --  apeend to activation chain
      if Chain_Data /= null then
         T.Next_Of_Activation_Chain := Chain_Data.List;
         Chain_Data.List := T;
         Chain_Data.Task_Count := Chain_Data.Task_Count + 1;
      end if;
      --  append to the parent's master
      if Parent /= null then
         Append_To_Completion_List (Master_Of_Parent, T);
      end if;
      --  try create
      Error := C.pthread.pthread_create (
         T.Handle'Access,
         null,
         Thread'Access,
         C.void_ptr (Task_Record_Conv.To_Address (T)));
      T.Handle_Received := True;
      if Error /= 0 then
         if Chain_Data /= null then
            Chain_Data.Task_Count := Chain_Data.Task_Count - 1;
         end if;
         Free (T); -- and remove from parent's master
         raise Tasking_Error;
      end if;
   end Create;

   procedure Wait (T : in out Task_Id) is
   begin
      if T /= null then
         declare
            T2 : constant Task_Id := T;
            Rec : aliased C.void_ptr;
         begin
            T := null; -- clear before raising any exception
            Joining : loop
               declare
                  Handle_Received : constant Boolean := T2.Handle_Received;
               begin
                  case C.pthread.pthread_join (T2.Handle, Rec'Access) is
                     when 0 =>
                        exit Joining;
                     when C.errno.ESRCH =>
                        if Handle_Received then
                           raise Tasking_Error;
                        end if;
                        Yield;
                        --  if the thread of caling Create and
                        --  the thread of calling Wait are different,
                        --  retry when handle is not received
                     when others =>
                        raise Tasking_Error;
                  end case;
               end;
            end loop Joining;
            if Rec /= C.void_ptr (Null_Address) then
               declare
                  Returned_T : Task_Id :=
                     Task_Record_Conv.To_Pointer (To_Address (Rec));
               begin
                  Free (Returned_T);
               end;
            end if;
         end;
      end if;
   end Wait;

   procedure Detach (T : in out Task_Id) is
   begin
      if T /= null then
         if sync_bool_compare_and_swap (
            T.State'Access,
            TS_Active,
            TS_Detached)
         then
            declare
               Handle : constant C.pthread.pthread_t := T.Handle;
            begin
               T := null;
               if C.pthread.pthread_detach (Handle) /= 0 then
                  raise Tasking_Error;
               end if;
            end;
         else
            Wait (T); -- release by caller
         end if;
      end if;
   end Detach;

   function Terminated (T : Task_Id) return Boolean is
   begin
      if T = null then
         raise Program_Error; -- RM C.7.1(15)
      elsif T.Kind = Main then
         return False;
      else
         return T.State = TS_Terminated;
      end if;
   end Terminated;

   function Activated (T : Task_Id) return Boolean is
   begin
      if T = null then
         raise Program_Error; -- RM C.7.1(15)
      else
         return T.Activated;
      end if;
   end Activated;

   procedure Accept_Activation is
      T : constant Task_Id := Get_Current_Task_Id;
      C : Activation_Chain_Access := T.Activation_Chain;
   begin
      pragma Assert (C /= null);
      Enter (C.Mutex);
      C.Activated_Count := C.Activated_Count + 1;
      if C.Activated_Count > C.Task_Count then
         Notify_All (C.Condition_Variable);
      else
         Wait (C.Condition_Variable, C.Mutex);
      end if;
      T.Activated := True;
      Leave (C.Mutex);
      --  cleanup
      if sync_sub_and_fetch (C.Activated_Count'Access, 1) = 0 then
         Free (C);
      end if;
   end Accept_Activation;

   procedure Activate (Chain : not null access Activation_Chain) is
      C : Activation_Chain_Access :=
         Activation_Chain_Conv.To_Pointer (Chain.Data);
   begin
      while C /= null loop
         Enter (C.Mutex);
         C.Activated_Count := C.Activated_Count + 1;
         if C.Activated_Count > C.Task_Count then
            Notify_All (C.Condition_Variable);
         else
            Wait (C.Condition_Variable, C.Mutex);
         end if;
         Leave (C.Mutex);
         --  cleanup
         declare
            Merged : constant Activation_Chain_Access := C.Merged;
         begin
            Chain.Data := Null_Address;
            if sync_sub_and_fetch (C.Activated_Count'Access, 1) = 0 then
               Free (C);
            end if;
            C := Merged;
         end;
      end loop;
   end Activate;

   procedure Move (
      From, To : not null access Activation_Chain;
      New_Master : Master_Level) is
   begin
      if From.Data /= Null_Address then
         --  change master of tasks
         declare
            I : Task_Id := Activation_Chain_Conv.To_Pointer (From.Data).List;
         begin
            while I /= null loop
               --  master
               I.Master_Level := New_Master;
               pragma Assert (I.Master_Top = null);
               --  activation
               I.Activation_Chain :=
                  Activation_Chain_Conv.To_Pointer (To.Data);
               --  completion
               if I.Master_Of_Parent /= null then
                  declare
                     New_Master_Of_Parent : Master_Access :=
                        I.Master_Of_Parent;
                  begin
                     Remove_From_Completion_List (I);
                     while New_Master_Of_Parent.Within > New_Master loop
                        New_Master_Of_Parent := New_Master_Of_Parent.Previous;
                     end loop;
                     Append_To_Completion_List (New_Master_Of_Parent, I);
                  end;
               end if;
               --  continue
               I := I.Next_Of_Activation_Chain;
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

   function Parent (T : Task_Id) return Task_Id is
   begin
      return T.Parent;
   end Parent;

   function Master_Level_Of (T : Task_Id) return Master_Level is
   begin
      return T.Master_Level;
   end Master_Level_Of;

   function Master_Within (T : Task_Id) return Master_Level is
   begin
      if T.Master_Top = null then
         return T.Master_Level + 1;
      else
         return T.Master_Top.Within;
      end if;
   end Master_Within;

   procedure Enter_Master (T : Task_Id) is
      New_Master : constant Master_Access := new Master_Data'(
         Previous => T.Master_Top,
         Within =>
            Master_Level'Max (Master_Within (T), Library_Task_Level) + 1,
         List => null,
         Mutex => (Handle => C.pthread.PTHREAD_MUTEX_INITIALIZER));
   begin
      T.Master_Top := New_Master;
   end Enter_Master;

   procedure Leave_Master (T : Task_Id) is
      M : Master_Access := T.Master_Top;
   begin
      Enter (M.Mutex);
      while M.List /= null loop
         declare
            Taken : Task_Id := M.List;
         begin
            Leave (M.Mutex);
            Wait (Taken);
            Enter (M.Mutex);
         end;
      end loop;
      Leave (M.Mutex);
      declare
         Previous : constant Master_Access := M.Previous;
      begin
         Free (M);
         T.Master_Top := Previous;
      end;
   end Leave_Master;

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
      Enter (Attribute_Indexes_Lock);
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
      declare
         P : constant Natural := Index.Index / Word'Size;
         B : constant Natural := Index.Index mod Word'Size;
      begin
         Attribute_Indexes (P) := Attribute_Indexes (P) and not (2 ** B);
      end;
      Finalize (Index.Mutex);
      Leave (Attribute_Indexes_Lock);
   end Free;

   function Get (T : Task_Id; Index : Attribute_Index) return Address is
      Result : Address;
   begin
      Enter (Index.Mutex'Unrestricted_Access.all);
      if T.Attributes_Length > Index.Index
         and then T.Attributes (Index.Index).Index = Index'Unrestricted_Access
      then
         Result := T.Attributes (Index.Index).Item;
      else
         Result := Null_Address;
      end if;
      Leave (Index.Mutex'Unrestricted_Access.all);
      return Result;
   end Get;

   procedure Set (
      T : Task_Id;
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
      T : Task_Id;
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
      T : Task_Id;
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
               A.Next.Attributes (Index.Index).Next := A.Previous;
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
      Timeout : Duration;
      Notified : out Boolean)
   is
      function Cast is new Ada.Unchecked_Conversion (Duration, Time_Rep);
      Now : aliased C.sys.time.struct_timeval;
      abstime : aliased C.sys.time.struct_timespec;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.sys.time.gettimeofday (Now'Access, null);
      abstime.tv_sec := Now.tv_sec
         + C.sys.types.time_t (Cast (Timeout) / 1000_000_000);
      abstime.tv_nsec := C.signed_long (Now.tv_usec) * 1000
         + C.signed_long (Cast (Timeout) mod 1000_000_000);
      if abstime.tv_nsec >= 1000_000_000 then
         abstime.tv_sec := abstime.tv_sec + 1;
         abstime.tv_nsec := abstime.tv_nsec - 1000_000_000;
      end if;
      --  wait
      case C.pthread.pthread_cond_timedwait (
         Object.Handle'Access,
         Mutex.Handle'Access,
         abstime'Access)
      is
         when 0 =>
            Notified := True;
         when C.errno.ETIMEDOUT =>
            Notified := False;
         when others =>
            raise Tasking_Error;
      end case;
   end Wait;

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
         Wait (Object.Condition_Variable, Object.Mutex);
      end if;
      Leave (Object.Mutex);
   end Wait;

   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean) is
   begin
      Enter (Object.Mutex);
      if Object.Value then
         Value := True;
      else
         Wait (Object.Condition_Variable, Object.Mutex, Timeout, Value);
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
      Notified : out Boolean) is
   begin
      Enter (Object.Mutex);
      Notified := Object.Blocked = 0;
      Object.Blocked := Object.Blocked + 1;
      if Object.Blocked = Object.Release_Threshold then
         Notify_All (Object.Condition_Variable);
         Object.Blocked := 0;
      else
         Wait (Object.Condition_Variable, Object.Mutex);
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
