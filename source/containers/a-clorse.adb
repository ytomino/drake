with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Comparators;
package body Ada.Containers.Limited_Ordered_Sets is
   use type Binary_Trees.Node_Access;
--  diff

   function Upcast is new Unchecked_Conversion (
      Cursor,
      Binary_Trees.Node_Access);
   function Downcast is new Unchecked_Conversion (
      Binary_Trees.Node_Access,
      Cursor);

--  diff (Upcast)
--
--
--  diff (Downcast)
--
--

   function Compare is new Comparators.Generic_Compare (Element_Type);

   function Compare_Node (Left, Right : not null Binary_Trees.Node_Access)
      return Integer;
   function Compare_Node (Left, Right : not null Binary_Trees.Node_Access)
      return Integer is
   begin
      return Compare (
         Downcast (Left).Element.all,
         Downcast (Right).Element.all);
   end Compare_Node;

   function Find (
      Container : Set;
      Item : Element_Type;
      Mode : Binary_Trees.Find_Mode)
      return Cursor;
   function Find (
      Container : Set;
      Item : Element_Type;
      Mode : Binary_Trees.Find_Mode)
      return Cursor
   is
      function Compare (Right : not null Binary_Trees.Node_Access)
         return Integer;
      function Compare (Right : not null Binary_Trees.Node_Access)
         return Integer is
      begin
         return Compare (Item, Downcast (Right).Element.all);
      end Compare;
   begin
      return Downcast (Binary_Trees.Find (
         Container.Root,
         Mode,
         Compare => Compare'Access));
   end Find;

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

   procedure Free_Node (Object : in out Binary_Trees.Node_Access);
   procedure Free_Node (Object : in out Binary_Trees.Node_Access) is
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
      Binary_Trees.Free (
         Data.Root,
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

   function Ceiling (Container : Set; Item : Element_Type) return Cursor is
   begin
      return Find (Container, Item, Binary_Trees.Ceiling);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   end Ceiling;

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
      return (Element => Position.Element);
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
--

   procedure Delete (Container : in out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in out Set; Position  : in out Cursor) is
      Position_2 : Binary_Trees.Node_Access := Upcast (Position);
   begin
--  diff
      Base.Remove (
         Container.Root,
         Container.Length,
         Position_2);
      Free_Node (Position_2);
      Position := null;
   end Delete;

   procedure Difference (Target : in out Set; Source : Set) is
   begin
--  diff
--  diff
      Binary_Trees.Merge (
         Target.Root,
         Target.Length,
         Source.Root,
         In_Only_Left => True,
         In_Only_Right => False,
         In_Both => False,
         Compare => Compare_Node'Access,
         Copy => null,
         Insert => null,
         Remove => Base.Remove'Access,
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

--  diff (Element)
--
--
--

   function Empty_Set return Set is
   begin
      return (Finalization.Limited_Controlled with Root => null, Length => 0);
   end Empty_Set;

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean is
   begin
      return not (Left < Right) and then not (Right < Left);
   end Equivalent_Elements;

   function Equivalent_Sets (Left, Right : Set) return Boolean is
      function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
         return Boolean;
      function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
         return Boolean is
      begin
         return Equivalent_Elements (
            Downcast (Left).Element.all,
            Downcast (Right).Element.all);
      end Equivalent;
   begin
      return Left.Length = Right.Length
         and then Binary_Trees.Equivalent (
            Left.Root,
            Right.Root,
            Equivalent => Equivalent'Access);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
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
      return Find (Container, Item, Binary_Trees.Just);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   end Find;

   function First (Container : Set) return Cursor is
   begin
      return Downcast (Binary_Trees.First (Container.Root));
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   end First;

   function Floor (Container : Set; Item : Element_Type) return Cursor is
   begin
      return Find (Container, Item, Binary_Trees.Floor);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   end Floor;

--  diff (Generic_Array_To_Set)
--
--
--
--
--
--
--

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
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
      Position  : out Cursor)
--  diff
   is
      New_Element : Element_Access := new Element_Type'(New_Item (Container));
      Before : constant Cursor := Ceiling (Container, New_Element.all);
   begin
      if Before = null or else New_Element.all < Before.Element.all then
         Position := new Node'(
            Super => <>,
            Element => New_Element);
         Base.Insert (
            Container.Root,
            Container.Length,
            Upcast (Before),
            Upcast (Position));
--  diff
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
      Binary_Trees.Merge (
         Target.Root,
         Target.Length,
         Source.Root,
         In_Only_Left => False,
         In_Only_Right => False,
         In_Both => True,
         Compare => Compare_Node'Access,
         Copy => null,
         Insert => null,
         Remove => Base.Remove'Access,
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

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Root = null;
--  diff
   end Is_Empty;

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
--  diff
--  diff
--  diff
--  diff
--  diff
      return Binary_Trees.Is_Subset (
         Subset.Root,
         Of_Set.Root,
         Compare => Compare_Node'Access);
--  diff
   end Is_Subset;

   procedure Iterate (
      Container : Set;
      Process : not null access procedure (Position : Cursor))
   is
      procedure Process_2 (Position : not null Binary_Trees.Node_Access);
      procedure Process_2 (Position : not null Binary_Trees.Node_Access) is
      begin
         Process (Downcast (Position));
      end Process_2;
   begin
--  diff
--  diff
      Binary_Trees.Iterate (
         Container.Root,
         Process_2'Access);
--  diff
   end Iterate;

   function Last (Container : Set) return Cursor is
   begin
      return Downcast (Binary_Trees.Last (Container.Root));
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   end Last;

   function Length (Container : Set) return Count_Type is
   begin
      return Container.Length;
--  diff
--  diff
--  diff
--  diff
   end Length;

   procedure Move (Target : in out Set; Source : in out Set) is
   begin
      if Target.Root /= Source.Root then
         Clear (Target);
         Target.Root := Source.Root;
         Target.Length := Source.Length;
         Source.Root := null;
         Source.Length := 0;
      end if;
   end Move;

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Binary_Trees.Next (Upcast (Position)));
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Binary_Trees.Next (Upcast (Position)));
   end Next;

   function No_Element return Cursor is
   begin
      return null;
   end No_Element;

   function Overlap (Left, Right : Set) return Boolean is
   begin
--  diff
--  diff
--  diff
      return Binary_Trees.Overlap (
         Left.Root,
         Right.Root,
         Compare => Compare_Node'Access);
--  diff
   end Overlap;

   function Previous (Position : Cursor) return Cursor is
   begin
      return Downcast (Binary_Trees.Previous (Upcast (Position)));
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Downcast (Binary_Trees.Previous (Upcast (Position)));
   end Previous;

   procedure Query_Element (
      Position : Cursor;
      Process  : not null access procedure (Element : Element_Type)) is
   begin
      Process (Position.Element.all);
   end Query_Element;

   function Reference (
      Container : not null access Set;
      Position : Cursor)
      return Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element);
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

   procedure Reverse_Iterate (
      Container : Set;
      Process : not null access procedure (Position : Cursor))
   is
      procedure Process_2 (Position : not null Binary_Trees.Node_Access);
      procedure Process_2 (Position : not null Binary_Trees.Node_Access) is
      begin
         Process (Downcast (Position));
      end Process_2;
   begin
--  diff
--  diff
      Binary_Trees.Reverse_Iterate (
         Container.Root,
         Process_2'Access);
--  diff
   end Reverse_Iterate;

--  diff  (Symmetric_Difference)
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

--  diff  (Symmetric_Difference)
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

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Left.Element.all < Right.Element.all;
   end "<";

   function "<=" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= null and then not (Right.Element.all < Left.Element.all);
   end "<=";

   function ">=" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= null and then not (Left.Element.all < Right.Element.all);
   end ">=";

   package body Generic_Keys is

      function Compare is new Comparators.Generic_Compare (Key_Type);

      function Find (
         Container : Set;
         Key : Key_Type;
         Mode : Binary_Trees.Find_Mode) return Cursor;
      function Find (
         Container : Set;
         Key : Key_Type;
         Mode : Binary_Trees.Find_Mode) return Cursor
      is
         function Compare (Right : not null Binary_Trees.Node_Access)
            return Integer;
         function Compare (Right : not null Binary_Trees.Node_Access)
            return Integer is
         begin
            return Compare (
               Key,
               Generic_Keys.Key (Downcast (Right).Element.all));
         end Compare;
      begin
         return Downcast (Binary_Trees.Find (
            Container.Root,
            Mode,
            Compare => Compare'Access));
      end Find;

      function Ceiling (Container : Set; Key : Key_Type) return Cursor is
      begin
         return Find (Container, Key, Binary_Trees.Ceiling);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      end Ceiling;

      function Contains (Container : Set; Key : Key_Type) return Boolean is
      begin
         return Find (Container, Key) /= null;
      end Contains;

      procedure Delete (Container : in out Set; Key : Key_Type) is
         Position : Cursor := Find (Container, Key);
      begin
         Delete (Container, Position);
      end Delete;

--  diff (Element)
--
--
--

      function Equivalent_Keys (Left, Right : Key_Type) return Boolean is
      begin
         return not (Left < Right) and not (Right < Left);
      end Equivalent_Keys;

      procedure Exclude (Container : in out Set; Key : Key_Type) is
         Position : Cursor := Find (Container, Key);
      begin
         if Position /= null then
            Delete (Container, Position);
         end if;
      end Exclude;

      function Find (Container : Set; Key : Key_Type) return Cursor is
      begin
         return Find (Container, Key, Binary_Trees.Just);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      end Find;

      function Floor (Container : Set; Key : Key_Type) return Cursor is
      begin
         return Find (Container, Key, Binary_Trees.Floor);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      end Floor;

--  diff (Replace)
--
--
--
--
--
--

      function Key (Position : Cursor) return Key_Type is
      begin
         return Key (Position.Element.all);
      end Key;

      procedure Update_Element_Preserving_Key (
         Container : in out Set;
         Position  : Cursor;
         Process : not null access procedure (Element : in out Element_Type))
      is
         pragma Unreferenced (Container);
      begin
         Process (Position.Element.all);
      end Update_Element_Preserving_Key;

   end Generic_Keys;

   package body Equivalents is

      function "=" (Left, Right : Set) return Boolean is
         function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
            return Boolean;
         function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
            return Boolean is
         begin
            return Downcast (Left).Element.all = Downcast (Right).Element.all;
         end Equivalent;
      begin
         return Left.Length = Right.Length and then
            Binary_Trees.Equivalent (Left.Root,
                                     Right.Root,
                                     Equivalent'Access);
      end "=";

   end Equivalents;

end Ada.Containers.Limited_Ordered_Sets;
