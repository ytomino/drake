with Ada.Unchecked_Deallocation;
package body Ada.Containers.Forward_Iterators is
   pragma Check_Policy (Validate => Ignore);

   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Node, Node_Access);

   procedure Retain (Node : not null Node_Access);
   procedure Retain (Node : not null Node_Access) is
   begin
      Node.Reference_Count := Node.Reference_Count + 1;
   end Retain;

   procedure Release (Node : in out Node_Access);
   procedure Release (Node : in out Node_Access) is
      Item : Node_Access := Node;
   begin
      while Item /= null loop
         Item.Reference_Count := Item.Reference_Count - 1;
         exit when Item.Reference_Count > 0;
         declare
            Next : constant Node_Access := Item.Next;
         begin
            Free (Item.Item);
            Free (Item);
            Item := Next;
         end;
      end loop;
      Node := null;
   end Release;

   procedure Update_Last (Object : in out Iterator);
   procedure Update_Last (Object : in out Iterator) is
   begin
      if not Has_Element (Object.Last_Input_Cursor) then
         Object.State := No_Element;
         Release (Object.Last);
      else
         declare
            New_Node : constant Node_Access := new Node'(
               Reference_Count => 1,
               Next => null,
               Item => new Element_Type'(Element (Object.Last_Input_Cursor)));
         begin
            if Object.Last /= null then
               pragma Check (Validate, Object.Last.Next = null);
               Object.Last.Next := New_Node;
               Retain (New_Node);
               Release (Object.Last);
            end if;
            Object.Last := New_Node;
         end;
      end if;
   end Update_Last;

   --  implementation

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Reference (Position) /= null;
   end Has_Element;

   function Constant_Reference (Position : Cursor)
      return Constant_Reference_Type is
   begin
      return (Element => Reference (Position).Item.all'Access);
   end Constant_Reference;

   function Iterate return Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Result : Iterator := (Finalization.Limited_Controlled with
         Last_Input_Cursor => Input_Iterator_Interfaces.First (Input_Iterator),
         Last => null,
         State => First)
      do
         Update_Last (Result);
      end return;
   end Iterate;

   package body Controlled is

      function Create (Node : Node_Access) return Cursor is
      begin
         if Node /= null then
            Retain (Node);
         end if;
         return (Finalization.Controlled with Node => Node);
      end Create;

      function Reference (Position : Cursor) return Node_Access is
      begin
         return Position.Node;
      end Reference;

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

   end Controlled;

   overriding procedure Finalize (Object : in out Iterator) is
   begin
      Release (Object.Last);
   end Finalize;

   overriding function First (Object : Iterator) return Cursor is
      pragma Check (Pre,
         Check => Object.State = First or else raise Status_Error);
   begin
      return Create (Object.Last);
   end First;

   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor
   is
      Mutable_Object : Iterator
         renames Object'Unrestricted_Access.all;
   begin
      if Reference (Position).Next /= null then
         return Create (Reference (Position).Next);
      else
         --  Position is the current last
         if Mutable_Object.State /= No_Element then
            Mutable_Object.State := Next;
            Mutable_Object.Last_Input_Cursor :=
               Input_Iterator_Interfaces.Next (
                  Input_Iterator,
                  Mutable_Object.Last_Input_Cursor);
            Update_Last (Mutable_Object);
         end if;
         return Create (Mutable_Object.Last);
      end if;
   end Next;

end Ada.Containers.Forward_Iterators;
