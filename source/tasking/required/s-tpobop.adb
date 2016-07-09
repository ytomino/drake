with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Soft_Links;
with System.Synchronous_Objects.Abortable;
with System.Tasks;
package body System.Tasking.Protected_Objects.Operations is
   use Ada.Exception_Identification.From_Here;
   use type Ada.Exceptions.Exception_Id;
   use type Synchronous_Objects.Queue_Node_Access;

   function Filter (
      The_Node : not null Synchronous_Objects.Queue_Node_Access;
      Params : Address)
      return Boolean;
   function Filter (
      The_Node : not null Synchronous_Objects.Queue_Node_Access;
      Params : Address)
      return Boolean is
   begin
      return Entries.Downcast (The_Node).E = Protected_Entry_Index (Params);
   end Filter;

   package Task_Record_Conv is
      new Address_To_Named_Access_Conversions (
         Tasks.Task_Record,
         Tasks.Task_Id);

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

   procedure Uncall (
      Object : not null access Entries.Protection_Entries'Class;
      Item : not null Synchronous_Objects.Queue_Node_Access;
      Already_Taken : out Boolean);
   procedure Uncall (
      Object : not null access Entries.Protection_Entries'Class;
      Item : not null Synchronous_Objects.Queue_Node_Access;
      Already_Taken : out Boolean)
   is
      Taken : Synchronous_Objects.Queue_Node_Access;
   begin
      Synchronous_Objects.Take (
         Object.Calling,
         Taken,
         Queue_Node_Conv.To_Address (Item),
         Uncall_Filter'Access);
      Already_Taken := Taken = null;
      pragma Assert (Taken = null or else Taken = Item);
   end Uncall;

   function Invoke_Filter (
      The_Node : not null Synchronous_Objects.Queue_Node_Access;
      Params : Address)
      return Boolean;
   function Invoke_Filter (
      The_Node : not null Synchronous_Objects.Queue_Node_Access;
      Params : Address)
      return Boolean
   is
      --  The_Node to Entries.Node
      Node : constant not null Entries.Node_Access :=
         Entries.Downcast (The_Node);
      --  Params to access Entries.Protection_Entries'Class
      function To_Pointer (Value : Address)
         return access Entries.Protection_Entries'Class
         with Import, Convention => Intrinsic;
      Object : constant not null access Entries.Protection_Entries'Class :=
         To_Pointer (Params);
      Index : constant Positive_Protected_Entry_Index :=
         Object.Find_Body_Index (
            Object.Compiler_Info,
            Node.E);
      Result : Boolean;
   begin
      --  queue is locked in filter
      Node.Action := False;
      Node.Requeued := False;
      begin
         Result := Object.Entry_Bodies (Index).Barrier (
            Object.Compiler_Info,
            Node.E);
         Node.Action := Result; -- execute the body after removing node
      exception
         when others =>
            Object.Raised_On_Barrier := True;
            Result := True;
            --  C953001
            Ada.Exceptions.Save_Exception (Node.X, Program_Error'Identity);
      end;
      return Result;
   end Invoke_Filter;

   procedure Invoke (
      Object : not null access Entries.Protection_Entries'Class);
   procedure Invoke (
      Object : not null access Entries.Protection_Entries'Class)
   is
      function To_Address (Value : access Entries.Protection_Entries'Class)
         return Address
         with Import, Convention => Intrinsic;
      Taken : Synchronous_Objects.Queue_Node_Access;
   begin
      pragma Assert (Object.Entry_Bodies'First = 1);
      loop
         Synchronous_Objects.Unsynchronized_Take (
            Object.Calling,
            Taken,
            To_Address (Object),
            Invoke_Filter'Access);
         exit when Taken = null;
         declare
            Node : constant not null Entries.Node_Access :=
               Entries.Downcast (Taken);
         begin
            if Node.Action then
               declare
                  Index : constant Positive_Protected_Entry_Index :=
                     Object.Find_Body_Index (
                        Object.Compiler_Info,
                        Node.E);
               begin
                  Object.Current_Calling := Node;
                  begin
                     Object.Entry_Bodies (Index).Action (
                        Object.Compiler_Info,
                        Node.Uninterpreted_Data,
                        Node.E);
                  exception
                     when E : others =>
                        Ada.Exceptions.Save_Occurrence (Node.X, E);
                  end;
                  Object.Current_Calling := null;
               end;
            end if;
            if Node.Requeued then
               --  internal requeue is a part of a single protected operation.
               declare
                  Canceled : Boolean;
               begin
                  Synchronous_Objects.Unsynchronized_Prepend (
                     Object.Calling,
                     Taken,
                     Canceled);
                  if Canceled then -- it does not happen ?
                     Ada.Exceptions.Save_Exception (
                        Node.X,
                        Tasking_Error'Identity);
                  end if;
               end;
            else
               Synchronous_Objects.Set (Node.Waiting);
               if Object.Raised_On_Barrier then
                  --  cancel all current callers, RM 9.5.3(7/3)
                  Object.Raised_On_Barrier := False;
                  Entries.Unlock_Entries (Object);
                  Entries.Cancel_Calls (Object.all);
                  Entries.Lock_Entries (Object);
               end if;
            end if;
         end;
      end loop;
   end Invoke;

   --  implementation

   procedure Service_Entries (
      Object : not null access Entries.Protection_Entries'Class) is
   begin
      --  already locked
      Invoke (Object);
      Entries.Unlock_Entries (Object);
   end Service_Entries;

   procedure Complete_Entry_Body (
      Object : not null access Entries.Protection_Entries'Class) is
   begin
      null;
   end Complete_Entry_Body;

   procedure Exceptional_Complete_Entry_Body (
      Object : not null access Entries.Protection_Entries'Class;
      Id : Ada.Exceptions.Exception_Id)
   is
      Current_X : constant Ada.Exceptions.Exception_Occurrence_Access :=
         Soft_Links.Get_Current_Excep.all;
   begin
      pragma Assert (Ada.Exceptions.Exception_Identity (Current_X.all) = Id);
      Ada.Exceptions.Save_Occurrence (Object.Current_Calling.X, Current_X.all);
   end Exceptional_Complete_Entry_Body;

   procedure Protected_Entry_Call (
      Object : not null access Entries.Protection_Entries'Class;
      E : Protected_Entry_Index;
      Uninterpreted_Data : Address;
      Mode : Call_Modes;
      Block : out Communication_Block)
   is
      pragma Unreferenced (Block);
   begin
      case Mode is
         when Simple_Call =>
            declare
               package Holder is
                  new Ada.Exceptions.Finally.Scoped_Holder (
                     Synchronous_Objects.Event,
                     Synchronous_Objects.Finalize);
               The_Node : aliased Entries.Node := (
                  Super => <>,
                  E => E,
                  Uninterpreted_Data => Uninterpreted_Data,
                  Caller =>
                     Task_Record_Conv.To_Address (Tasks.Current_Task_Id),
                  Action => False,
                  Requeued => False,
                  Waiting => <>, -- default initializer
                  X => <>); -- default initializer
               Canceled : Boolean;
               Aborted : Boolean;
            begin
               Synchronous_Objects.Initialize (The_Node.Waiting);
               Holder.Assign (The_Node.Waiting);
               Synchronous_Objects.Add (
                  Object.Calling,
                  The_Node.Super'Unchecked_Access,
                  Canceled);
               if Canceled then
                  Raise_Exception (Tasking_Error'Identity);
               end if;
               Tasks.Enable_Abort;
               Entries.Lock_Entries (Object);
               Invoke (Object);
               Entries.Unlock_Entries (Object);
               Synchronous_Objects.Abortable.Wait (
                  The_Node.Waiting,
                  Aborted => Aborted);
               if Aborted then
                  declare
                     Already_Taken : Boolean;
                  begin
                     Uncall (
                        Object,
                        The_Node.Super'Unchecked_Access,
                        Already_Taken => Already_Taken);
                     if Already_Taken then
                        Synchronous_Objects.Wait (The_Node.Waiting);
                        --  without abort checking
                     end if;
                  end;
               end if;
               Tasks.Disable_Abort (Aborted); -- if aborted, raise here
               if Ada.Exceptions.Exception_Identity (The_Node.X) /=
                  Ada.Exceptions.Null_Id
               then
                  Ada.Exceptions.Reraise_Nonnull_Occurrence (The_Node.X);
               end if;
            end;
         when Conditional_Call =>
            raise Program_Error;
         when Asynchronous_Call =>
            raise Program_Error;
      end case;
   end Protected_Entry_Call;

   procedure Timed_Protected_Entry_Call (
      Object : not null access Entries.Protection_Entries'Class;
      E : Protected_Entry_Index;
      Uninterpreted_Data : Address;
      Timeout : Duration;
      Mode : Integer;
      Entry_Call_Successful : out Boolean) is
   begin
      raise Program_Error;
   end Timed_Protected_Entry_Call;

   function Enqueued (Block : Communication_Block) return Boolean is
   begin
      raise Program_Error;
      return Enqueued (Block);
   end Enqueued;

   function Cancelled (Block : Communication_Block) return Boolean is
   begin
      raise Program_Error;
      return Cancelled (Block);
   end Cancelled;

   procedure Cancel_Protected_Entry_Call (
      Block : in out Communication_Block) is
   begin
      raise Program_Error;
   end Cancel_Protected_Entry_Call;

   procedure Requeue_Protected_Entry (
      Object : not null access Entries.Protection_Entries'Class;
      New_Object : not null access Entries.Protection_Entries'Class;
      E : Protected_Entry_Index;
      With_Abort : Boolean) is
   begin
      if With_Abort then
         raise Program_Error;
      end if;
      if Object = New_Object then
         --  internal requeue
         Object.Current_Calling.E := E;
         Object.Current_Calling.Requeued := True;
      else
         raise Program_Error;
      end if;
   end Requeue_Protected_Entry;

   procedure Requeue_Task_To_Protected_Entry (
      New_Object : not null access Entries.Protection_Entries'Class;
      E : Protected_Entry_Index;
      With_Abort : Boolean) is
   begin
      raise Program_Error;
   end Requeue_Task_To_Protected_Entry;

   function Protected_Entry_Caller (
      Object : Entries.Protection_Entries'Class)
      return Task_Id is
   begin
      return Object.Current_Calling.Caller;
   end Protected_Entry_Caller;

   function Protected_Count (
      Object : Entries.Protection_Entries'Class;
      E : Protected_Entry_Index)
      return Natural is
   begin
      --  locked because 'Count is called only from barriers or bodies
      return Synchronous_Objects.Unsynchronized_Count (
         Object.Calling,
         System'To_Address (E),
         Filter'Access);
   end Protected_Count;

end System.Tasking.Protected_Objects.Operations;
