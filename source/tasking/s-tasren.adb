with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Soft_Links;
with System.Tasking.Inside;
package body System.Tasking.Rendezvous is
   pragma Suppress (All_Checks);
   use type Ada.Exceptions.Exception_Id;

   package Task_Record_Conv is new Address_To_Named_Access_Conversions (
      Inside.Task_Record,
      Inside.Task_Id);

   type Node;
   type Node_Access is access all Node;
   type Node is limited record
      Super : aliased Inside.Queue_Node;
      Previous : Node_Access;
      E : Task_Entry_Index;
      Uninterpreted_Data : Address;
      Waiting : aliased Inside.Event;
      X : Ada.Exceptions.Exception_Occurrence;
   end record;
   pragma Suppress_Initialization (Node);

   function Downcast is new Ada.Unchecked_Conversion (
      Inside.Queue_Node_Access,
      Node_Access);

   function Filter (
      The_Node : not null Inside.Queue_Node_Access;
      Params : Address)
      return Boolean;
   function Filter (
      The_Node : not null Inside.Queue_Node_Access;
      Params : Address)
      return Boolean is
   begin
      return Downcast (The_Node).E = Task_Entry_Index (Params);
   end Filter;

   procedure Cancel_Call (Call : not null Node_Access);
   procedure Cancel_Call (Call : not null Node_Access) is
   begin
      begin
         raise Tasking_Error;
      exception
         when E : Tasking_Error =>
            Ada.Exceptions.Save_Occurrence (Call.X, E);
      end;
      Inside.Set (Call.Waiting);
   end Cancel_Call;

   procedure Cancel_Call (X : in out Inside.Queue_Node_Access);
   procedure Cancel_Call (X : in out Inside.Queue_Node_Access) is
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
      Inside.Set (Current_Call.Waiting);
      TLS_Current_Call := Current_Call.Previous;
      Inside.Leave_Unabortable; -- Abort_Undefer will not be called by compiler
      Ada.Exceptions.Reraise_Occurrence (X);
   end Exceptional_Complete_Rendezvous;

   --  implementation

   procedure Accept_Call (
      E : Task_Entry_Index;
      Uninterpreted_Data : out Address)
   is
      The_Node : Inside.Queue_Node_Access;
      Aborted : Boolean;
   begin
      Inside.Enable_Abort;
      Inside.Accept_Call (
         The_Node,
         Address (E),
         Filter'Access,
         Aborted => Aborted);
      Inside.Disable_Abort (Aborted); -- if aborted, raise here
      Downcast (The_Node).Previous := TLS_Current_Call;
      TLS_Current_Call := Downcast (The_Node);
      Uninterpreted_Data := Downcast (The_Node).Uninterpreted_Data;
   end Accept_Call;

   procedure Complete_Rendezvous is
      Current_Call : constant Node_Access := TLS_Current_Call;
   begin
      Inside.Set (Current_Call.Waiting);
      TLS_Current_Call := Current_Call.Previous;
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

   procedure Call_Simple (
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      Uninterpreted_Data : Address) is
   begin
      if Inside.Elaborated (Task_Record_Conv.To_Pointer (Acceptor))
         and then not Inside.Activated (Task_Record_Conv.To_Pointer (Acceptor))
      then
         --  in extended return statement
         Inside.Activate (Task_Record_Conv.To_Pointer (Acceptor));
      end if;
      declare
         procedure Finally (X : not null access Inside.Event);
         procedure Finally (X : not null access Inside.Event) is
         begin
            Inside.Finalize (X.all);
         end Finally;
         package Holder is
            new Ada.Exceptions.Finally.Scoped_Holder (Inside.Event, Finally);
         The_Node : aliased Node := (
            Super => <>,
            Previous => null,
            E => E,
            Uninterpreted_Data => Uninterpreted_Data,
            Waiting => <>, -- default initializer
            X => <>); -- default initializer
         Aborted : Boolean;
      begin
         Inside.Initialize (The_Node.Waiting);
         Holder.Assign (The_Node.Waiting'Access);
         Inside.Call (
            Task_Record_Conv.To_Pointer (Acceptor),
            The_Node.Super'Unchecked_Access);
         Inside.Enable_Abort;
         Inside.Wait (The_Node.Waiting, Aborted => Aborted);
         if Aborted then
            declare
               Already_Taken : Boolean;
            begin
               Inside.Uncall (
                  Task_Record_Conv.To_Pointer (Acceptor),
                  The_Node.Super'Unchecked_Access,
                  Already_Taken => Already_Taken);
               if Already_Taken then
                  Inside.Wait (The_Node.Waiting); -- without abort checking
               end if;
            end;
         end if;
         Inside.Disable_Abort (Aborted); -- if aborted, raise here
         if Ada.Exceptions.Exception_Identity (The_Node.X) /=
            Ada.Exceptions.Null_Id
         then
            Ada.Exceptions.Reraise_Occurrence (The_Node.X);
         end if;
      end;
   end Call_Simple;

   function Callable (T : Task_Id) return Boolean is
   begin
      return Inside.Callable (Task_Record_Conv.To_Pointer (T));
   end Callable;

   function Task_Count (E : Task_Entry_Index) return Natural is
   begin
      return Inside.Call_Count (
         Inside.Current_Task_Id,
         Address (E),
         Filter'Access);
   end Task_Count;

begin
   Inside.Cancel_Call_Hook := Cancel_Call'Access;
end System.Tasking.Rendezvous;
