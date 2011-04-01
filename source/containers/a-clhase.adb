with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
package body Ada.Containers.Limited_Hashed_Sets is
   use type Hash_Tables.Table_Access;
--  diff

   function Upcast is new Unchecked_Conversion (
      Cursor,
      Hash_Tables.Node_Access);
   function Downcast is new Unchecked_Conversion (
      Hash_Tables.Node_Access,
      Cursor);

--  diff (Upcast)
--
--
--  diff (Downcast)
--
--

   type Context_Type is limited record
      Left : not null access Element_Type;
   end record;
   pragma Suppress_Initialization (Context_Type);

   function Equivalent_Element (
      Position : not null Hash_Tables.Node_Access;
      Params : System.Address)
      return Boolean;
   function Equivalent_Element (
      Position : not null Hash_Tables.Node_Access;
      Params : System.Address)
      return Boolean
   is
      Context : Context_Type;
      for Context'Address use Params;
   begin
      return Equivalent_Elements (
         Context.Left.all,
         Downcast (Position).Element.all);
   end Equivalent_Element;

   function Equivalent_Node (Left, Right : not null Hash_Tables.Node_Access)
      return Boolean;
   function Equivalent_Node (Left, Right : not null Hash_Tables.Node_Access)
      return Boolean is
   begin
      return Equivalent_Elements (
         Downcast (Left).Element.all,
         Downcast (Right).Element.all);
   end Equivalent_Node;

--  diff (Copy_Node)
--
--
--
--
--
--
--
--
--
--
--

   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Node, Cursor);

   procedure Free_Node (Object : in out Hash_Tables.Node_Access);
   procedure Free_Node (Object : in out Hash_Tables.Node_Access) is
      X : Cursor := Downcast (Object);
   begin
      Free (X.Element);
      Free (X);
      Object := null;
   end Free_Node;

--  diff (Allocation_Data)
--
--
--
--
--
--
--
--
--
--
--

--  diff (Copy_Data)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Free)

   procedure Free_Data (Data : in out Set);
   procedure Free_Data (Data : in out Set) is
--  diff
   begin
      Hash_Tables.Free (
         Data.Table,
         Data.Length,
         Free => Free_Node'Access);
--  diff
--  diff
   end Free_Data;

--  diff (Unique)
--
--
--
--
--
--
--
--
--

   function Find (Container : Set; Hash : Hash_Type; Item : Element_Type)
      return Cursor;
   function Find (Container : Set; Hash : Hash_Type; Item : Element_Type)
      return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
--  diff
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Hash_Tables.Find (
               Container.Table,
               Hash,
               Context'Address,
               Equivalent => Equivalent_Element'Access));
         end;
      end if;
   end Find;

--  diff (Adjust)
--
--
--

--  diff (Assign)
--
--
--
--
--
--

   function Capacity (Container : Set) return Count_Type is
   begin
      return Hash_Tables.Capacity (Container.Table);
--  diff
--  diff
--  diff
--  diff
--  diff
   end Capacity;

   procedure Clear (Container : in out Set) is
   begin
      Free_Data (Container);
--  diff
--  diff
   end Clear;

   function Constant_Reference (
      Container : not null access constant Set;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Constant_Reference;

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= null;
   end Contains;

--  diff (Copy)
--
--
--
--
--
--

   procedure Delete (Container : in out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in out Set; Position : in out Cursor) is
   begin
--  diff
      Hash_Tables.Remove (
         Container.Table,
         Container.Length,
         Upcast (Position));
      Free (Position);
   end Delete;

   procedure Difference (Target : in out Set; Source : Set) is
   begin
--  diff
--  diff
      Hash_Tables.Merge (
         Target.Table,
         Target.Length,
         Source.Table,
         Source.Length,
         In_Only_Left => True,
         In_Only_Right => False,
         In_Both => False,
         Equivalent => Equivalent_Node'Access,
         Copy => null,
         Free => Free_Node'Access);
--  diff
   end Difference;

--  diff (Difference)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Element)
--
--
--

   function Empty_Set return Set is
   begin
      return (Finalization.Limited_Controlled with Table => null, Length => 0);
   end Empty_Set;

   function Equivalent_Elements (Left : Cursor; Right : Element_Type)
      return Boolean is
   begin
      return Equivalent_Elements (Left.Element.all, Right);
   end Equivalent_Elements;

   function Equivalent_Sets (Left, Right : Set) return Boolean is
   begin
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      return Hash_Tables.Equivalent (
         Left.Table,
         Left.Length,
         Right.Table,
         Right.Length,
         Equivalent => Equivalent_Node'Access);
--  diff
   end Equivalent_Sets;

   procedure Exclude (Container : in out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      if Position /= null then
         Delete (Container, Position);
      end if;
   end Exclude;

   function Find (Container : Set; Item : Element_Type) return Cursor is
   begin
      return Find (Container, Hash (Item), Item);
   end Find;

   function First (Container : Set) return Cursor is
   begin
      return Downcast (Hash_Tables.First (Container.Table));
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   end First;

   function First (Object : Iterator) return Cursor is
   begin
      return First (Object.all);
   end First;

--  diff (Generic_Array_To_Set)
--
--
--
--
--
--
--
--

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
   end Has_Element;

--  diff (Include)
--
--
--
--
--
--
--
--

   procedure Insert (
      Container : in out Set;
      New_Item : not null access function (C : Set) return Element_Type;
      Position : out Cursor)
   is
      New_Element : Element_Access := new Element_Type'(New_Item (Container));
      New_Hash : constant Hash_Type := Hash (New_Element.all);
   begin
      if Find (Container, New_Hash, New_Element.all) = null then
         Position := new Node'(
            Super => <>,
            Element => New_Element);
         Hash_Tables.Insert (
            Container.Table,
            Container.Length,
            New_Hash,
            Upcast (Position));
      else
         Free (New_Element);
         raise Constraint_Error;
      end if;
   end Insert;

--  diff (Insert)
--
--
--
--
--
--
--
--

   procedure Intersection (Target : in out Set; Source : Set) is
   begin
--  diff
--  diff
--  diff
--  diff
      Hash_Tables.Merge (
         Target.Table,
         Target.Length,
         Source.Table,
         Source.Length,
         In_Only_Left => False,
         In_Only_Right => False,
         In_Both => True,
         Equivalent => Equivalent_Node'Access,
         Copy => null,
         Free => Free_Node'Access);
--  diff
   end Intersection;

--  diff (Intersection)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Length = 0;
--  diff
   end Is_Empty;

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
--  diff
--  diff
--  diff
--  diff
--  diff
      return Hash_Tables.Is_Subset (
         Subset.Table,
         Of_Set.Table,
         Equivalent => Equivalent_Node'Access);
--  diff
   end Is_Subset;

   procedure Iterate (
      Container : Set;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Hash_Tables.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
--  diff
--  diff
      Hash_Tables.Iterate (
         Container.Table,
         Cast (Process));
--  diff
   end Iterate;

   function Iterate (Container : not null access constant Set)
      return Iterator is
   begin
      return Iterator (Container);
   end Iterate;

   function Length (Container : Set) return Count_Type is
   begin
--  diff
--  diff
--  diff
      return Container.Length;
--  diff
   end Length;

   procedure Move (Target : in out Set; Source : in out Set) is
   begin
      Clear (Target);
      Target.Table := Source.Table;
      Target.Length := Source.Length;
      Source.Table := null;
      Source.Length := 0;
   end Move;

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Next);
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Next);
   end Next;

   function Next (Object : Iterator; Position : Cursor) return Cursor is
      pragma Unreferenced (Object);
   begin
      return Next (Position);
   end Next;

   function Overlap (Left, Right : Set) return Boolean is
   begin
--  diff
--  diff
--  diff
      return Hash_Tables.Overlap (
         Left.Table,
         Right.Table,
         Equivalent => Equivalent_Node'Access);
--  diff
   end Overlap;

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Position.Element.all);
   end Query_Element;

   function Reference (Container : not null access Set; Position : Cursor)
      return Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Reference;

--  diff (Replace)
--
--
--

--  diff (Replace_Element)
--
--
--
--
--
--
--
--

   procedure Reserve_Capacity (
      Container : in out Set;
      Capacity : Count_Type)
   is
      New_Capacity : constant Count_Type :=
         Count_Type'Max (Capacity, Length (Container));
   begin
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      Hash_Tables.Rebuild (
         Container.Table,
         New_Capacity);
   end Reserve_Capacity;

--  diff (Symmetric_Difference)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Symmetric_Difference)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (To_Set)
--
--
--
--
--

--  diff (Union)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Union)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff ("=")
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

   package body Generic_Keys is

      type Context_Type is limited record
         Left : not null access Key_Type;
      end record;
      pragma Suppress_Initialization (Context_Type);

      function Equivalent_Key (
         Position : not null Hash_Tables.Node_Access;
         Params : System.Address)
         return Boolean;
      function Equivalent_Key (
         Position : not null Hash_Tables.Node_Access;
         Params : System.Address)
         return Boolean
      is
         Context : Context_Type;
         for Context'Address use Params;
      begin
         return Equivalent_Keys (
            Context.Left.all,
            Key (Downcast (Position).Element.all));
      end Equivalent_Key;

      function Contains (Container : Set; Key : Key_Type) return Boolean is
      begin
         return Find (Container, Key) /= null;
      end Contains;

--  diff (Element)
--
--
--

      procedure Exclude (Container : in out Set; Key : Key_Type) is
         Position : Cursor := Find (Container, Key);
      begin
         if Position /= null then
            Delete (Container, Position);
         end if;
      end Exclude;

      function Find (Container : Set; Key : Key_Type) return Cursor is
      begin
         if Is_Empty (Container) then
            return null;
         else
--  diff
            declare
               Context : Context_Type := (Left => Key'Unrestricted_Access);
            begin
               return Downcast (Hash_Tables.Find (
                  Container.Table,
                  Hash (Key),
                  Context'Address,
                  Equivalent => Equivalent_Key'Access));
            end;
         end if;
      end Find;

      procedure Delete (Container : in out Set; Key : Key_Type) is
         Position : Cursor := Find (Container, Key);
      begin
         Delete (Container, Position);
      end Delete;

      function Key (Position : Cursor) return Key_Type is
      begin
         return Key (Position.Element.all);
      end Key;

--  diff (Replace)
--
--
--
--
--
--

      procedure Update_Element_Preserving_Key (
         Container : in out Set;
         Position : Cursor;
         Process : not null access procedure (Element : in out Element_Type))
      is
         pragma Unreferenced (Container);
      begin
         Process (Position.Element.all);
      end Update_Element_Preserving_Key;

   end Generic_Keys;

   package body Equivalents is

      function "=" (Left, Right : Set) return Boolean is
         function Equivalent (Left, Right : not null Hash_Tables.Node_Access)
            return Boolean;
         function Equivalent (Left, Right : not null Hash_Tables.Node_Access)
            return Boolean is
         begin
            return Downcast (Left).Element.all = Downcast (Right).Element.all;
         end Equivalent;
      begin
         return Hash_Tables.Equivalent (
            Left.Table,
            Left.Length,
            Right.Table,
            Right.Length,
            Equivalent => Equivalent'Access);
      end "=";

   end Equivalents;

end Ada.Containers.Limited_Hashed_Sets;
