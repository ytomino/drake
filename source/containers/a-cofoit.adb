with Ada.Exceptions.Finally;
with Ada.Unchecked_Deallocation;
package body Ada.Containers.Forward_Iterators is

   procedure Free is new Unchecked_Deallocation (Queue, Queue_Access);
   procedure Free is new Unchecked_Deallocation (Node, Node_Access);

   procedure Retain (Node : not null Node_Access);
   procedure Retain (Node : not null Node_Access) is
   begin
      Node.Reference_Count := Node.Reference_Count + 1;
   end Retain;

   procedure Release (Node : in out Node_Access);
   procedure Release (Node : in out Node_Access) is
   begin
      if Node /= null then
         Node.Reference_Count := Node.Reference_Count - 1;
         if Node.Reference_Count = 0 then
            Release (Node.Next);
            Free (Node);
         end if;
      end if;
   end Release;

   function Get_Next (Queue : not null Queue_Access) return Cursor;
   function Get_Next (Queue : not null Queue_Access) return Cursor is
   begin
      if End_Of_File (Queue.File.all) then
         return (Finalization.Controlled with Node => null); -- No_Element
      else
         declare
            New_Node : aliased Node_Access := new Node'(
               Reference_Count => 1,
               Next => null,
               Element => <>);
            procedure Finally (X : not null access Node_Access);
            procedure Finally (X : not null access Node_Access) is
            begin
               Free (X.all);
            end Finally;
            package Holder is new Exceptions.Finally.Scoped_Holder (
               Node_Access,
               Finally);
         begin
            Holder.Assign (New_Node'Access);
            Get (Queue.File.all, New_Node.Element);
            if Queue.Last /= null then
               pragma Assert (Queue.Last.Next = null);
               Queue.Last.Next := New_Node;
               Retain (New_Node);
            end if;
            Release (Queue.Last);
            Queue.Last := New_Node;
            Retain (New_Node);
            Holder.Clear;
            return (Finalization.Controlled with Node => New_Node);
         end;
      end if;
   end Get_Next;

   --  implementation

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Node /= null;
   end Has_Element;

   function Constant_Reference (
      Container : File_Type;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Node.Element'Access);
   end Constant_Reference;

   function Iterate (Container : not null access File_Type) return Iterator is
      New_Queue : constant Queue_Access := new Queue'(
         File => Container,
         Last => null,
         Next_Called => False);
   begin
      return (Finalization.Limited_Controlled with New_Queue);
   end Iterate;

   overriding procedure Adjust (Object : in out Cursor) is
   begin
      if Object.Node /= null then
         Retain (Object.Node);
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Cursor) is
   begin
      Release (Object.Node);
   end Finalize;

   overriding procedure Finalize (Object : in out Iterator) is
   begin
      if Object.Queue /= null then
         Release (Object.Queue.Last);
         Free (Object.Queue);
      end if;
   end Finalize;

   function First (Object : Iterator'Class) return Cursor is
   begin
      if Object.Queue = null or else Object.Queue.Next_Called then
         raise Status_Error;
      end if;
      if Object.Queue.Last /= null then
         Retain (Object.Queue.Last);
         return (Finalization.Controlled with Node => Object.Queue.Last);
      else
         return Get_Next (Object.Queue);
      end if;
   end First;

   function Next (Object : Iterator'Class; Position : Cursor) return Cursor is
   begin
      if Position.Node.Next /= null then
         Retain (Position.Node.Next);
         return (Finalization.Controlled with Node => Position.Node.Next);
      else
         return Result : constant Cursor := Get_Next (Object.Queue) do
            if Result.Node /= null then
               Object.Queue.Next_Called := True;
            end if;
         end return;
      end if;
   end Next;

end Ada.Containers.Forward_Iterators;
