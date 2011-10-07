with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Soft_Links;
with System.Tasking.Inside;
package body System.Tasking.Rendezvous is
   use type Ada.Exceptions.Exception_Id;

   package Task_Record_Conv is new Address_To_Named_Access_Conversions (
      Inside.Task_Record,
      Inside.Task_Id);

   type Node is limited record
      Super : aliased Inside.Queue_Node;
      E : Task_Entry_Index;
      Uninterpreted_Data : Address;
      Waiting : Inside.Event;
      X : Ada.Exceptions.Exception_Occurrence;
   end record;
   pragma Suppress_Initialization (Node);
   type Node_Access is access all Node;

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

   procedure Cancel_Call (X : in out Inside.Queue_Node_Access);
   procedure Cancel_Call (X : in out Inside.Queue_Node_Access) is
      Call : constant not null Node_Access := Downcast (X);
   begin
      begin
         raise Tasking_Error;
      exception
         when E : Tasking_Error =>
            Ada.Exceptions.Save_Occurrence (Call.X, E);
      end;
      Inside.Set (Call.Waiting);
   end Cancel_Call;

   TLS_Current_Call : Node_Access := null;
   pragma Thread_Local_Storage (TLS_Current_Call);

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
      TLS_Current_Call := Downcast (The_Node);
      Uninterpreted_Data := Downcast (The_Node).Uninterpreted_Data;
   end Accept_Call;

   procedure Complete_Rendezvous is
   begin
      Inside.Set (TLS_Current_Call.Waiting);
      TLS_Current_Call := null;
   end Complete_Rendezvous;

   procedure Exceptional_Complete_Rendezvous (
      Ex : Ada.Exceptions.Exception_Id)
   is
      Current_Call : constant Node_Access := TLS_Current_Call;
      Occ : Ada.Exceptions.Exception_Occurrence
         renames Soft_Links.Get_Current_Excep.all.all;
   begin
      pragma Assert (Ada.Exceptions.Exception_Identity (Occ) = Ex);
      Ada.Exceptions.Save_Occurrence (Current_Call.X, Occ);
      Inside.Set (Current_Call.Waiting);
      TLS_Current_Call := null;
      Ada.Exceptions.Reraise_Occurrence (Occ);
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
      if not Inside.Activated (Task_Record_Conv.To_Pointer (Acceptor)) then
         --  in extended return statement
         Inside.Activate (
            Task_Record_Conv.To_Pointer (Acceptor),
            Final => False); -- for Move_Activation_Chain
      end if;
      declare
         The_Node : aliased Node := (
            Super => <>,
            E => E,
            Uninterpreted_Data => Uninterpreted_Data,
            Waiting => <>, -- default initializer
            X => <>); -- default initializer
         Aborted : Boolean;
      begin
         Inside.Enable_Abort;
         Inside.Initialize (The_Node.Waiting);
         Inside.Call (
            Task_Record_Conv.To_Pointer (Acceptor),
            The_Node.Super'Unchecked_Access);
         Inside.Wait (The_Node.Waiting, Aborted => Aborted);
         Inside.Finalize (The_Node.Waiting);
         Inside.Disable_Abort (Aborted); -- if aborted, raise here
         if Ada.Exceptions.Exception_Identity (The_Node.X)
            /= Ada.Exceptions.Null_Id
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
         Inside.Get_Current_Task_Id,
         Address (E),
         Filter'Access);
   end Task_Count;

begin
   Inside.Cancel_Call_Hook := Cancel_Call'Access;
end System.Tasking.Rendezvous;
