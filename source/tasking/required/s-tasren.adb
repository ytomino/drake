with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Soft_Links;
with System.Synchronous_Objects.Abortable;
with System.Tasks;
package body System.Tasking.Rendezvous is
   use Ada.Exception_Identification.From_Here;
   use type Ada.Exceptions.Exception_Id;

   package Task_Record_Conv is
      new Address_To_Named_Access_Conversions (
         Tasks.Task_Record,
         Tasks.Task_Id);

   type Node;
   type Node_Access is access all Node;
   type Node is limited record
      Super : aliased Synchronous_Objects.Queue_Node;
      Previous : Node_Access;
      E : Task_Entry_Index;
      Uninterpreted_Data : Address;
      Caller : Task_Id;
      Waiting : aliased Synchronous_Objects.Event;
      X : Ada.Exceptions.Exception_Occurrence;
   end record;
   pragma Suppress_Initialization (Node);

   function Downcast is
      new Ada.Unchecked_Conversion (
         Synchronous_Objects.Queue_Node_Access,
         Node_Access);

   function Filter (
      The_Node : not null Synchronous_Objects.Queue_Node_Access;
      Params : Address)
      return Boolean;
   function Filter (
      The_Node : not null Synchronous_Objects.Queue_Node_Access;
      Params : Address)
      return Boolean is
   begin
      return Downcast (The_Node).E = Task_Entry_Index (Params);
   end Filter;

   procedure Cancel_Call (Call : not null Node_Access);
   procedure Cancel_Call (Call : not null Node_Access) is
   begin
      Ada.Exceptions.Save_Exception_From_Here (Call.X, Tasking_Error'Identity);
      Synchronous_Objects.Set (Call.Waiting);
   end Cancel_Call;

   procedure Cancel_Call (X : in out Synchronous_Objects.Queue_Node_Access);
   procedure Cancel_Call (X : in out Synchronous_Objects.Queue_Node_Access) is
      Call : constant not null Node_Access := Downcast (X);
   begin
      Cancel_Call (Call);
   end Cancel_Call;

   TLS_Current_Call : Node_Access := null;
   pragma Thread_Local_Storage (TLS_Current_Call);

   procedure Exceptional_Complete_Rendezvous (
      X : Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Exceptional_Complete_Rendezvous);

   procedure Exceptional_Complete_Rendezvous (
      X : Ada.Exceptions.Exception_Occurrence)
   is
      Current_Call : constant Node_Access := TLS_Current_Call;
   begin
      Ada.Exceptions.Save_Occurrence (Current_Call.X, X);
      TLS_Current_Call := Current_Call.Previous;
      Synchronous_Objects.Set (Current_Call.Waiting);
      if not ZCX_By_Default then
         Tasks.Unlock_Abort;
         --  Abort_Undefer will not be called by compiler
      end if;
      Ada.Exceptions.Unchecked_Reraise_Occurrence (X);
   end Exceptional_Complete_Rendezvous;

   --  implementation

   procedure Accept_Call (
      E : Task_Entry_Index;
      Uninterpreted_Data : out Address)
   is
      The_Node : Synchronous_Objects.Queue_Node_Access;
      Aborted : Boolean;
   begin
      Tasks.Enable_Abort;
      Tasks.Accept_Call (
         The_Node,
         System'To_Address (E),
         Filter'Access,
         Aborted => Aborted);
      Tasks.Disable_Abort (Aborted); -- if aborted, raise here
      Downcast (The_Node).Previous := TLS_Current_Call;
      TLS_Current_Call := Downcast (The_Node);
      Uninterpreted_Data := Downcast (The_Node).Uninterpreted_Data;
   end Accept_Call;

   procedure Complete_Rendezvous is
      Current_Call : constant Node_Access := TLS_Current_Call;
   begin
      TLS_Current_Call := Current_Call.Previous;
      Synchronous_Objects.Set (Current_Call.Waiting);
   end Complete_Rendezvous;

   procedure Exceptional_Complete_Rendezvous (
      Ex : Ada.Exceptions.Exception_Id)
   is
      Occ : Ada.Exceptions.Exception_Occurrence
         renames Soft_Links.Get_Current_Excep.all.all;
   begin
      pragma Assert (Ada.Exceptions.Exception_Identity (Occ) = Ex);
      Exceptional_Complete_Rendezvous (Occ);
   end Exceptional_Complete_Rendezvous;

   procedure Accept_Trivial (E : Task_Entry_Index) is
      Dummy : Address;
      pragma Unreferenced (Dummy);
   begin
      Accept_Call (E, Dummy);
      Complete_Rendezvous;
   end Accept_Trivial;

   procedure Task_Entry_Call (
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      Uninterpreted_Data : Address;
      Mode : Call_Modes;
      Rendezvous_Successful : out Boolean) is
   begin
      raise Program_Error;
   end Task_Entry_Call;

   procedure Timed_Task_Entry_Call (
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      Uninterpreted_Data : Address;
      Timeout : Duration;
      Mode : Integer;
      Rendezvous_Successful : out Boolean) is
   begin
      raise Program_Error;
   end Timed_Task_Entry_Call;

   procedure Call_Simple (
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      Uninterpreted_Data : Address) is
   begin
      if Tasks.Elaborated (Task_Record_Conv.To_Pointer (Acceptor))
         and then not Tasks.Activated (Task_Record_Conv.To_Pointer (Acceptor))
      then
         --  in extended return statement
         Tasks.Activate (Task_Record_Conv.To_Pointer (Acceptor));
      end if;
      declare
         procedure Finally (X : not null access Synchronous_Objects.Event);
         procedure Finally (X : not null access Synchronous_Objects.Event) is
         begin
            Synchronous_Objects.Finalize (X.all);
         end Finally;
         package Holder is
            new Ada.Exceptions.Finally.Scoped_Holder (
               Synchronous_Objects.Event,
               Finally);
         The_Node : aliased Node := (
            Super => <>,
            Previous => null,
            E => E,
            Uninterpreted_Data => Uninterpreted_Data,
            Caller => Task_Record_Conv.To_Address (Tasks.Current_Task_Id),
            Waiting => <>, -- default initializer
            X => <>); -- default initializer
         Aborted : Boolean;
      begin
         Synchronous_Objects.Initialize (The_Node.Waiting);
         Holder.Assign (The_Node.Waiting'Access);
         Tasks.Call (
            Task_Record_Conv.To_Pointer (Acceptor),
            The_Node.Super'Unchecked_Access);
         Tasks.Enable_Abort;
         Synchronous_Objects.Abortable.Wait (
            The_Node.Waiting,
            Aborted => Aborted);
         if Aborted then
            declare
               Already_Taken : Boolean;
            begin
               Tasks.Uncall (
                  Task_Record_Conv.To_Pointer (Acceptor),
                  The_Node.Super'Unchecked_Access,
                  Already_Taken => Already_Taken);
               if Already_Taken then
                  Synchronous_Objects.Wait (The_Node.Waiting);
                  --  without abort checking
               end if;
            end;
         end if;
         Tasks.Disable_Abort (Aborted); -- if aborted, raise here
         declare
            X_Id : constant Ada.Exceptions.Exception_Id :=
               Ada.Exceptions.Exception_Identity (The_Node.X);
         begin
            if X_Id /= Ada.Exceptions.Null_Id then
               if X_Id = Standard'Abort_Signal'Identity then
                  Raise_Exception (Tasking_Error'Identity); -- C9A011A
               else
                  Ada.Exceptions.Unchecked_Reraise_Occurrence (The_Node.X);
               end if;
            end if;
         end;
      end;
   end Call_Simple;

   procedure Cancel_Task_Entry_Call (Cancelled : out Boolean) is
   begin
      raise Program_Error;
   end Cancel_Task_Entry_Call;

   procedure Requeue_Task_Entry (
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      With_Abort : Boolean) is
   begin
      raise Program_Error;
   end Requeue_Task_Entry;

   procedure Requeue_Protected_To_Task_Entry (
      Object : not null access
         Protected_Objects.Entries.Protection_Entries'Class;
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      With_Abort : Boolean) is
   begin
      raise Program_Error;
   end Requeue_Protected_To_Task_Entry;

   function Callable (T : Task_Id) return Boolean is
   begin
      return Tasks.Callable (Task_Record_Conv.To_Pointer (T));
   end Callable;

   function Task_Entry_Caller (D : Task_Entry_Nesting_Depth) return Task_Id is
      Call : not null Node_Access := TLS_Current_Call;
   begin
      for I in 1 .. D loop
         Call := Call.Previous;
      end loop;
      return Call.Caller;
   end Task_Entry_Caller;

   function Task_Count (E : Task_Entry_Index) return Natural is
   begin
      return Tasks.Call_Count (
         Tasks.Current_Task_Id,
         System'To_Address (E),
         Filter'Access);
   end Task_Count;

begin
   Tasks.Cancel_Call_Hook := Cancel_Call'Access;
end System.Tasking.Rendezvous;
