with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
package body Ada.Containers.Limited_Doubly_Linked_Lists is
   use type Linked_Lists.Node_Access;
--  diff

   function Upcast is
      new Unchecked_Conversion (Cursor, Linked_Lists.Node_Access);
   function Downcast is
      new Unchecked_Conversion (Linked_Lists.Node_Access, Cursor);

--  diff (Upcast)
--
--  diff (Downcast)
--

   procedure Free is new Unchecked_Deallocation (Node, Cursor);
   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);

--  diff (Context_Type)
--
--
--

--  diff (Equivalent_Element)
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

--  diff (Allocate_Element)
--
--
--
--
--
--
--
--

--  diff (Allocate_Node)
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
--

   procedure Free_Node (Object : in out Linked_Lists.Node_Access);
   procedure Free_Node (Object : in out Linked_Lists.Node_Access) is
      X : Cursor := Downcast (Object);
   begin
      Free (X.Element);
      Free (X);
      Object := null;
   end Free_Node;

--  diff (Allocate_Data)
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
--
--
--
--
--
--

--  diff (Free)

   procedure Free_Data (Data : in out List);
   procedure Free_Data (Data : in out List) is
--  diff
   begin
      Linked_Lists.Free (
         Data.First,
         Data.Last,
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
--
--

   --  implementation

--  diff (Assign)
--
--
--
--
--
--

--  diff (Append)
--
--
--
--
--
--

   procedure Clear (Container : in out List) is
   begin
      Free_Data (Container);
--  diff
--  diff
   end Clear;

   function Constant_Reference (
      Container : aliased List;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Constant_Reference;

--  diff (Contains)
--
--
--

--  diff (Copy)
--
--
--
--
--
--
--
--
--

   procedure Delete (
      Container : in out List;
      Position : in out Cursor;
      Count : Count_Type := 1)
   is
      X : Linked_Lists.Node_Access;
      Next : Linked_Lists.Node_Access;
   begin
--  diff
      for I in 1 .. Count loop
         X := Upcast (Position);
         Next := Position.Super.Next;
         Base.Remove (
            Container.First,
            Container.Last,
            Container.Length,
            Position => X,
            Next => Next);
         Free_Node (X);
         Position := Downcast (Next);
      end loop;
   end Delete;

   procedure Delete_First (Container : in out List; Count : Count_Type := 1) is
      Position : Cursor;
   begin
      for I in 1 .. Count loop
         Position := Downcast (Container.First);
         Delete (Container, Position);
      end loop;
   end Delete_First;

   procedure Delete_Last (Container : in out List; Count : Count_Type := 1) is
      Position : Cursor;
   begin
      for I in 1 .. Count loop
         Position := Downcast (Container.Last);
         Delete (Container, Position);
      end loop;
   end Delete_Last;

--  diff (Element)
--
--
--

   function Empty_List return List is
   begin
      return (Finalization.Limited_Controlled with null, null, 0);
   end Empty_List;

--  diff (Find)
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

--  diff (Find)
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

   function First (Container : List) return Cursor is
   begin
      return Downcast (Container.First);
--  diff
--  diff
--  diff
--  diff
--  diff
   end First;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
   end Has_Element;

--  diff (Insert)
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

   procedure Insert (
      Container : in out List;
      Before : Cursor;
      New_Item : not null access function return Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
--  diff
      for I in 1 .. Count loop
         declare
            procedure Finally (X : not null access Cursor);
            procedure Finally (X : not null access Cursor) is
            begin
               Free (X.all);
            end Finally;
            package Holder is
               new Exceptions.Finally.Scoped_Holder (Cursor, Finally);
            X : aliased Cursor := new Node;
         begin
            Holder.Assign (X'Access);
            X.Element := new Element_Type'(New_Item.all);
            Holder.Clear;
            Base.Insert (
               Container.First,
               Container.Last,
               Container.Length,
               Before => Upcast (Before),
               New_Item => Upcast (X));
            Position := X;
         end;
      end loop;
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
--

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container.Last = null;
--  diff
   end Is_Empty;

   procedure Iterate (
      Container : List'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Linked_Lists.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
--  diff
--  diff
      Base.Iterate (
         Container.First,
         Cast (Process));
--  diff
   end Iterate;

   function Iterate (Container : List)
      return List_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return List_Iterator'(
         First => First (Container),
         Last => Last (Container));
   end Iterate;

   function Iterate (Container : List; First, Last : Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'Class
   is
      pragma Unreferenced (Container);
   begin
      if Base.Is_Before (Upcast (Last), Upcast (First)) then
         return List_Iterator'(First => No_Element, Last => No_Element);
      else
         return List_Iterator'(First => First, Last => Last);
      end if;
   end Iterate;

   function Last (Container : List) return Cursor is
   begin
      return Downcast (Container.Last);
--  diff
--  diff
--  diff
--  diff
--  diff
   end Last;

   function Length (Container : List) return Count_Type is
   begin
      return Container.Length;
--  diff
--  diff
--  diff
--  diff
   end Length;

   procedure Move (Target : in out List; Source : in out List) is
   begin
      if Target.First /= Source.First then
         Clear (Target);
         Target.First := Source.First;
         Target.Last := Source.Last;
         Target.Length := Source.Length;
         Source.First := null;
         Source.Last := null;
         Source.Length := 0;
      end if;
   end Move;

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Next);
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Next);
   end Next;

--  diff (Prepend)
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

   function Previous (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Super.Previous);
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Super.Previous);
   end Previous;

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Position.Element.all);
   end Query_Element;

   function Reference (
      Container : aliased in out List;
      Position : Cursor)
      return Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Reference;

--  diff (Replace_Element)
--
--
--
--
--
--
--
--

   procedure Reverse_Elements (Container : in out List) is
   begin
--  diff
--  diff
      Linked_Lists.Reverse_Elements (
         Container.First,
         Container.Last,
         Container.Length,
         Insert => Base.Insert'Access,
         Remove => Base.Remove'Access);
--  diff
   end Reverse_Elements;

--  diff (Reverse_Find)
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

--  diff (Reverse_Find)
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

   procedure Reverse_Iterate (
      Container : List;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Linked_Lists.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
--  diff
--  diff
      Linked_Lists.Reverse_Iterate (
         Container.Last,
         Cast (Process));
--  diff
   end Reverse_Iterate;

   procedure Splice (
      Target : in out List;
      Before : Cursor;
      Source : in out List) is
--  diff
--  diff
--  diff
   begin
      if Target.First /= Source.First then
--  diff
--  diff
         Base.Splice (
            Target.First,
            Target.Last,
            Target.Length,
            Upcast (Before),
            Source.First,
            Source.Last,
            Source.Length);
      end if;
   end Splice;

   procedure Splice (
      Target : in out List;
      Before : Cursor;
      Source : in out List;
      Position : in out Cursor) is
   begin
--  diff
--  diff
      Base.Remove (
         Source.First,
         Source.Last,
         Source.Length,
         Upcast (Position),
         Position.Super.Next);
      Base.Insert (
         Target.First,
         Target.Last,
         Target.Length,
         Upcast (Before),
         Upcast (Position));
   end Splice;

   procedure Splice (
      Container : in out List;
      Before : Cursor;
      Position : Cursor) is
   begin
--  diff
      Base.Remove (
         Container.First,
         Container.Last,
         Container.Length,
         Upcast (Position),
         Position.Super.Next);
      Base.Insert (
         Container.First,
         Container.Last,
         Container.Length,
         Upcast (Before),
         Upcast (Position));
   end Splice;

   procedure Swap (Container : in out List; I, J : Cursor) is
      pragma Unreferenced (Container);
--  diff
--  diff
      Temp : constant Element_Access := I.Element;
   begin
      I.Element := J.Element;
      J.Element := Temp;
--  diff
   end Swap;

   procedure Swap_Links (Container : in out List; I, J : Cursor) is
   begin
--  diff
      Base.Swap_Links (
         Container.First,
         Container.Last,
         Upcast (I),
         Upcast (J));
   end Swap_Links;

   procedure Update_Element (
      Container : in out List'Class;
      Position : Cursor;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (Container.Reference (Position).Element.all);
   end Update_Element;

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
--

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Base.Is_Before (Upcast (Left), Upcast (Right));
   end "<";

--  diff (Adjust)
--
--
--

   overriding function First (Object : List_Iterator) return Cursor is
   begin
      return Object.First;
   end First;

   overriding function Next (Object : List_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.Last then
         return No_Element;
      else
         return Next (Position);
      end if;
   end Next;

   overriding function Last (Object : List_Iterator) return Cursor is
   begin
      return Object.Last;
   end Last;

   overriding function Previous (Object : List_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.First then
         return No_Element;
      else
         return Previous (Position);
      end if;
   end Previous;

   package body Generic_Sorting is

      function LT (Left, Right : not null Linked_Lists.Node_Access)
         return Boolean;
      function LT (Left, Right : not null Linked_Lists.Node_Access)
         return Boolean is
      begin
         return Downcast (Left).Element.all <
            Downcast (Right).Element.all;
      end LT;

      function Is_Sorted (Container : List) return Boolean is
      begin
         return Linked_Lists.Is_Sorted (Container.Last, LT'Access);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      end Is_Sorted;

      procedure Sort (Container : in out List) is
      begin
--  diff
--  diff
         Linked_Lists.Merge_Sort (
            Container.First,
            Container.Last,
            Container.Length,
            LT => LT'Access,
            Splice => Base.Splice'Access,
            Split => Base.Split'Access,
            Insert => Base.Insert'Access,
            Remove => Base.Remove'Access);
--  diff
      end Sort;

      procedure Merge (Target : in out List; Source : in out List) is
      begin
         if not Is_Empty (Source) then
            if Is_Empty (Target) then
               Move (Target, Source);
            else
--  diff
--  diff
               Linked_Lists.Merge (
                  Target.First,
                  Target.Last,
                  Target.Length,
                  Source.First,
                  Source.Last,
                  Source.Length,
                  LT => LT'Access,
                  Insert => Base.Insert'Access,
                  Remove => Base.Remove'Access);
            end if;
         end if;
      end Merge;

   end Generic_Sorting;

   package body Equivalents is

      type Context_Type is limited record
         Left : not null access Element_Type;
      end record;
      pragma Suppress_Initialization (Context_Type);

      function Equivalent_Element (
         Right : not null Linked_Lists.Node_Access;
         Params : System.Address)
         return Boolean;
      function Equivalent_Element (
         Right : not null Linked_Lists.Node_Access;
         Params : System.Address)
         return Boolean
      is
         Context : Context_Type;
         for Context'Address use Params;
      begin
         return Context.Left.all = Downcast (Right).Element.all;
      end Equivalent_Element;

      function Contains (Container : List; Item : Element_Type)
         return Boolean is
      begin
         return Find (Container, Item) /= null;
      end Contains;

      function Find (Container : List; Item : Element_Type) return Cursor is
         Context : Context_Type := (Left => Item'Unrestricted_Access);
      begin
         return Downcast (Base.Find (
            Container.First,
            Context'Address,
            Equivalent => Equivalent_Element'Access));
      end Find;

      function Find (Container : List; Item : Element_Type; Position : Cursor)
         return Cursor
      is
         pragma Unreferenced (Container);
         Context : Context_Type := (Left => Item'Unrestricted_Access);
      begin
         return Downcast (Base.Find (
            Upcast (Position),
            Context'Address,
            Equivalent => Equivalent_Element'Access));
      end Find;

      function Reverse_Find (Container : List; Item : Element_Type)
         return Cursor
      is
         Context : Context_Type := (Left => Item'Unrestricted_Access);
      begin
         return Downcast (Linked_Lists.Reverse_Find (
            Container.Last,
            Context'Address,
            Equivalent => Equivalent_Element'Access));
      end Reverse_Find;

      function Reverse_Find (
         Container : List;
         Item : Element_Type;
         Position : Cursor)
         return Cursor
      is
         pragma Unreferenced (Container);
         Context : Context_Type := (Left => Item'Unrestricted_Access);
      begin
         return Downcast (Linked_Lists.Reverse_Find (
            Upcast (Position),
            Context'Address,
            Equivalent => Equivalent_Element'Access));
      end Reverse_Find;

      function "=" (Left, Right : List) return Boolean is
         function Equivalent (Left, Right : not null Linked_Lists.Node_Access)
            return Boolean;
         function Equivalent (Left, Right : not null Linked_Lists.Node_Access)
            return Boolean is
         begin
            return Downcast (Left).Element.all = Downcast (Right).Element.all;
         end Equivalent;
      begin
         return Left.Length = Right.Length
            and then
               Linked_Lists.Equivalent (
                  Left.Last,
                  Right.Last,
                  Equivalent'Access);
      end "=";

   end Equivalents;

end Ada.Containers.Limited_Doubly_Linked_Lists;
