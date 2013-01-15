pragma Check_Policy (Validate, Off);
with Ada.Unchecked_Deallocation;
package body Ada.Containers.Forward_Iterators is

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

   procedure Append (
      Object : in out Iterator;
      Result : out Cursor;
      Original : Input_Cursor);
   procedure Append (
      Object : in out Iterator;
      Result : out Cursor;
      Original : Input_Cursor) is
   begin
      if not Has_Element (Original) then
         Object.State := No_Element;
         pragma Check (Validate, Reference (Result) = null);
      else
         declare
            New_Node : constant Node_Access := new Node'(
               Reference_Count => 1,
               Next => null,
               Original => Original);
         begin
            if Object.Last /= null then
               pragma Check (Validate, Object.Last.Next = null);
               Object.Last.Next := New_Node;
               Retain (New_Node);
            end if;
            Release (Object.Last);
            Object.Last := New_Node;
            Retain (New_Node);
            Assign (Result, New_Node);
         end;
      end if;
   end Append;

   --  implementation

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Reference (Position) /= null;
   end Has_Element;

   function To (Position : Cursor) return Input_Cursor_Ref is
   begin
      return (Element => Reference (Position).Original'Access);
   end To;

   function Satisfy (
      Iterator : aliased in out
         Input_Iterator_Interfaces.Forward_Iterator'Class)
      return Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Forward_Iterators.Iterator'(Finalization.Limited_Controlled with
         Input_Iterator => Iterator'Unchecked_Access,
         Last => null,
         State => First);
   end Satisfy;

   package body Cursors is

      function Reference (Position : Cursor) return Node_Access is
      begin
         return Position.Node;
      end Reference;

      procedure Assign (Position : in out Cursor; Node : Node_Access) is
      begin
         Retain (Node);
         Position.Node := Node;
      end Assign;

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

   end Cursors;

   overriding procedure Finalize (Object : in out Iterator) is
   begin
      Release (Object.Last);
   end Finalize;

   overriding function First (Object : Iterator) return Cursor is
      Mutable_Object : Iterator renames Object'Unrestricted_Access.all;
   begin
      if Mutable_Object.State /= First then
         raise Status_Error;
      end if;
      return Result : Cursor do
         if Mutable_Object.Last /= null then
            Assign (Result, Mutable_Object.Last);
         else
            Append (
               Mutable_Object,
               Result,
               Input_Iterator_Interfaces.First (
                  Mutable_Object.Input_Iterator.all));
         end if;
      end return;
   end First;

   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor
   is
      Mutable_Object : Iterator renames Object'Unrestricted_Access.all;
   begin
      return Result : Cursor do
         if Reference (Position).Next /= null then
            Assign (Result, Reference (Position).Next);
         elsif Mutable_Object.State /= No_Element then
            Append (
               Mutable_Object,
               Result,
               Input_Iterator_Interfaces.Next (
                  Mutable_Object.Input_Iterator.all,
                  Reference (Position).Original));
            if Reference (Result) /= null then
               Mutable_Object.State := Next;
            end if;
         end if;
      end return;
   end Next;

end Ada.Containers.Forward_Iterators;
