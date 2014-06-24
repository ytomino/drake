with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Soft_Links;
with System.Synchronous_Control;
with System.Synchronous_Objects.Abortable;
package body System.Tasking.Protected_Objects.Operations is
   use type Ada.Exceptions.Exception_Id;
   use type Synchronous_Objects.Queue_Node_Access;

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
         return access Entries.Protection_Entries'Class;
      pragma Import (Intrinsic, To_Pointer);
      Object : constant not null access Entries.Protection_Entries'Class :=
         To_Pointer (Params);
      Index : constant Positive_Protected_Entry_Index :=
         Object.Find_Body_Index (
            Object.Compiler_Info,
            Node.E);
      Result : Boolean;
      Action : Boolean;
   begin
      --  queue is locked in filter
      Node.Requeued := False;
      begin
         Result := Object.Entry_Bodies (Index).Barrier (
            Object.Compiler_Info,
            Node.E);
         Action := Result;
      exception
         when others =>
            Object.Raised_On_Barrier := True;
            Result := True;
            Action := False;
            begin
               raise Program_Error; -- C953001
            exception
               when E : Program_Error =>
                  Ada.Exceptions.Save_Occurrence (Node.X, E);
            end;
      end;
      if Action then
         Object.Current_Calling := Node;
         Object.Entry_Bodies (Index).Action (
            Object.Compiler_Info,
            Node.Uninterpreted_Data,
            Node.E);
         Object.Current_Calling := null;
      end if;
      return Result;
   end Invoke_Filter;

   procedure Invoke (
      Object : not null access Entries.Protection_Entries'Class);
   procedure Invoke (
      Object : not null access Entries.Protection_Entries'Class)
   is
      function To_Address (Value : access Entries.Protection_Entries'Class)
         return Address;
      pragma Import (Intrinsic, To_Address);
      Taken : Synchronous_Objects.Queue_Node_Access;
   begin
      pragma Assert (Object.Entry_Bodies'First = 1);
      loop
         Synchronous_Objects.Take (
            Object.Calling,
            Taken,
            To_Address (Object),
            Invoke_Filter'Access);
         exit when Taken = null;
         declare
            Node : constant not null Entries.Node_Access :=
               Entries.Downcast (Taken);
         begin
            if Node.Requeued then
               Synchronous_Objects.Add (Object.Calling, Taken);
            else
               Synchronous_Objects.Set (Node.Waiting);
               if Object.Raised_On_Barrier then
                  Entries.Cancel_Calls (Object.all);
               end if;
            end if;
         end;
      end loop;
   end Invoke;

   --  implementation

   procedure Service_Entries (
      Object : not null access Entries.Protection_Entries'Class) is
   begin
      Entries.Unlock_Entries (Object);
      Invoke (Object);
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
      procedure Finally (X : not null access Synchronous_Objects.Event);
      procedure Finally (X : not null access Synchronous_Objects.Event) is
      begin
         Synchronous_Objects.Finalize (X.all);
      end Finally;
   begin
      case Mode is
         when Simple_Call =>
            declare
               package Holder is
                  new Ada.Exceptions.Finally.Scoped_Holder (
                     Synchronous_Objects.Event,
                     Finally);
               The_Node : aliased Entries.Node := (
                  Super => <>,
                  E => E,
                  Uninterpreted_Data => Uninterpreted_Data,
                  Requeued => False,
                  Waiting => <>, -- default initializer
                  X => <>); -- default initializer
               Aborted : Boolean;
            begin
               Synchronous_Objects.Initialize (The_Node.Waiting);
               Holder.Assign (The_Node.Waiting'Access);
               Synchronous_Objects.Add (
                  Object.Calling,
                  The_Node.Super'Unchecked_Access);
               Synchronous_Control.Unlock_Abort; -- Tasks.Enable_Abort;
               Invoke (Object);
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
               Synchronous_Control.Lock_Abort; -- Disable_Abort (Aborted);
               if Aborted then
                  delay 0.0; -- if aborted, raise here
               end if;
               if Ada.Exceptions.Exception_Identity (The_Node.X) /=
                  Ada.Exceptions.Null_Id
               then
                  Ada.Exceptions.Reraise_Occurrence (The_Node.X);
               end if;
            end;
         when Conditional_Call =>
            raise Program_Error;
         when Asynchronous_Call =>
            raise Program_Error;
      end case;
   end Protected_Entry_Call;

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

   function Protected_Count (
      Object : Entries.Protection_Entries'Class;
      E : Protected_Entry_Index)
      return Natural is
   begin
      raise Program_Error;
      return Protected_Count (Object, E);
   end Protected_Count;

end System.Tasking.Protected_Objects.Operations;
