with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
--  diff (Ada.Streams)
with System;
package body Ada.Containers.Limited_Ordered_Sets is
   use type Binary_Trees.Node_Access;
--  diff

   function Upcast is
      new Unchecked_Conversion (Cursor, Binary_Trees.Node_Access);
   function Downcast is
      new Unchecked_Conversion (Binary_Trees.Node_Access, Cursor);

--  diff (Upcast)
--
--  diff (Downcast)
--

   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Node, Cursor);

   function Compare_Elements (Left, Right : Element_Type) return Integer;
   function Compare_Elements (Left, Right : Element_Type) return Integer is
   begin
      if Left < Right then
         return -1;
      elsif Right < Left then
         return 1;
      else
         return 0;
      end if;
   end Compare_Elements;

   type Context_Type is limited record
      Left : not null access Element_Type;
   end record;
   pragma Suppress_Initialization (Context_Type);

   function Compare_Element (
      Position : not null Binary_Trees.Node_Access;
      Params : System.Address)
      return Integer;
   function Compare_Element (
      Position : not null Binary_Trees.Node_Access;
      Params : System.Address)
      return Integer
   is
      Context : Context_Type;
      for Context'Address use Params;
   begin
      return Compare_Elements (
         Context.Left.all,
         Downcast (Position).Element.all);
   end Compare_Element;

   function Compare_Node (Left, Right : not null Binary_Trees.Node_Access)
      return Integer;
   function Compare_Node (Left, Right : not null Binary_Trees.Node_Access)
      return Integer is
   begin
      return Compare_Elements (
         Downcast (Left).Element.all,
         Downcast (Right).Element.all);
   end Compare_Node;

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
--
--
--
--
--
--
--

   function Equivalent_Sets (
      Left, Right : Set;
      Equivalent : not null access function (
         Left, Right : not null Binary_Trees.Node_Access)
         return Boolean)
      return Boolean;
   function Equivalent_Sets (
      Left, Right : Set;
      Equivalent : not null access function (
         Left, Right : not null Binary_Trees.Node_Access)
         return Boolean)
      return Boolean
   is
      Left_Length : constant Count_Type := Length (Left);
      Right_Length : constant Count_Type := Length (Right);
   begin
      if Left_Length /= Right_Length then
         return False;
      elsif Left_Length = 0 then
         return True;
      else
--  diff
--  diff
         return Binary_Trees.Equivalent (
            Left.Root,
            Right.Root,
            Equivalent => Equivalent);
      end if;
   end Equivalent_Sets;

   --  implementation

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean is
   begin
      return Compare_Elements (Left, Right) = 0;
   end Equivalent_Elements;

   function Empty_Set return Set is
   begin
      return (Finalization.Limited_Controlled with Root => null, Length => 0);
   end Empty_Set;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

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
      return Equivalent_Sets (Left, Right, Equivalent => Equivalent'Access);
   end Equivalent_Sets;

--  diff (To_Set)
--
--
--
--
--

--  diff (Generic_Array_To_Set)
--
--
--
--
--
--
--

   function Length (Container : Set) return Count_Type is
--  diff
   begin
--  diff
--  diff
--  diff
      return Container.Length;
--  diff
   end Length;

   function Is_Empty (Container : Set) return Boolean is
--  diff
   begin
      return Container.Root = null;
   end Is_Empty;

   procedure Clear (Container : in out Set) is
   begin
      Free_Data (Container);
--  diff
--  diff
   end Clear;

--  diff (Element)
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

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Position.Element.all);
   end Query_Element;

   function Constant_Reference (
      Container : aliased Set;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Constant_Reference;

--  diff (Assign)
--
--
--
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
--
--

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

   procedure Insert (
      Container : in out Set;
      New_Item : not null access function return Element_Type;
      Position : out Cursor;
      Inserted : out Boolean)
   is
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Element_Access, Free);
      New_Element : aliased Element_Access := new Element_Type'(New_Item.all);
      Before : constant Cursor := Ceiling (Container, New_Element.all);
   begin
      Holder.Assign (New_Element);
      Inserted := Before = null or else New_Element.all < Before.Element.all;
      if Inserted then
         Position := new Node'(Super => <>, Element => New_Element);
         Holder.Clear;
--  diff
--  diff
--  diff
         Base.Insert (
            Container.Root,
            Container.Length,
            Upcast (Before),
            Upcast (Position));
--  diff
      else
         Position := Before;
      end if;
   end Insert;

   procedure Insert (
      Container : in out Set;
      New_Item : not null access function return Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, New_Item, Position, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

--  diff (Include)
--
--
--
--
--
--
--
--

--  diff (Replace)
--
--
--

   procedure Exclude (Container : in out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      if Position /= null then
         Delete (Container, Position);
      end if;
   end Exclude;

   procedure Delete (Container : in out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in out Set; Position : in out Cursor) is
      Position_2 : Binary_Trees.Node_Access := Upcast (Position);
   begin
--  diff
--  diff
--  diff
--  diff
      Base.Remove (Container.Root, Container.Length, Position_2);
--  diff
      Free_Node (Position_2);
      Position := null;
   end Delete;

   procedure Delete_First (Container : in out Set'Class) is
      Position : Cursor := First (Set (Container));
   begin
      Delete (Set (Container), Position);
   end Delete_First;

   procedure Delete_Last (Container : in out Set'Class) is
      Position : Cursor := Last (Set (Container));
   begin
      Delete (Set (Container), Position);
   end Delete_Last;

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
--

   procedure Intersection (Target : in out Set; Source : Set) is
   begin
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      Base.Merge (
         Target.Root,
         Target.Length,
         Source.Root,
         (Binary_Trees.In_Both => True, others => False),
         Compare => Compare_Node'Access,
         Copy => null,
         Free => Free_Node'Access);
--  diff
--  diff
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
--

   procedure Difference (Target : in out Set; Source : Set) is
   begin
--  diff
--  diff
--  diff
--  diff
--  diff
      Base.Merge (
         Target.Root,
         Target.Length,
         Source.Root,
         (Binary_Trees.In_Only_Left => True, others => False),
         Compare => Compare_Node'Access,
         Copy => null,
         Free => Free_Node'Access);
--  diff
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
--

   function Overlap (Left, Right : Set) return Boolean is
   begin
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      return Binary_Trees.Overlap (
         Left.Root,
         Right.Root,
         Compare => Compare_Node'Access);
--  diff
   end Overlap;

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
--  diff
--  diff
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

--  diff (First_Element)
--
--
--
--

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

--  diff (Last_Element)
--
--
--
--

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Binary_Trees.Next (Upcast (Position)));
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Binary_Trees.Next (Upcast (Position)));
   end Next;

   function Previous (Position : Cursor) return Cursor is
   begin
      return Downcast (Binary_Trees.Previous (Upcast (Position)));
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Downcast (Binary_Trees.Previous (Upcast (Position)));
   end Previous;

   function Find (Container : Set; Item : Element_Type) return Cursor is
   begin
--  diff
--  diff
--  diff
--  diff
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Binary_Trees.Find (
               Container.Root,
               Binary_Trees.Just,
               Context'Address,
               Compare => Compare_Element'Access));
         end;
--  diff
   end Find;

   function Floor (Container : Set; Item : Element_Type) return Cursor is
   begin
--  diff
--  diff
--  diff
--  diff
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Binary_Trees.Find (
               Container.Root,
               Binary_Trees.Floor,
               Context'Address,
               Compare => Compare_Element'Access));
         end;
--  diff
   end Floor;

   function Ceiling (Container : Set; Item : Element_Type) return Cursor is
   begin
--  diff
--  diff
--  diff
--  diff
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Binary_Trees.Find (
               Container.Root,
               Binary_Trees.Ceiling,
               Context'Address,
               Compare => Compare_Element'Access));
         end;
--  diff
   end Ceiling;

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= null;
   end Contains;

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= Right and then Left.Element.all < Right.Element.all;
   end "<";

   function ">" (Left, Right : Cursor) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<" (Left : Cursor; Right : Element_Type) return Boolean is
   begin
      return Left.Element.all < Right;
   end "<";

   function ">" (Left : Cursor; Right : Element_Type) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<" (Left : Element_Type; Right : Cursor) return Boolean is
   begin
      return Left < Right.Element.all;
   end "<";

   function ">" (Left : Element_Type; Right : Cursor) return Boolean is
   begin
      return Right < Left;
   end ">";

   procedure Iterate (
      Container : Set'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Binary_Trees.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
--  diff
--  diff
      Binary_Trees.Iterate (
         Container.Root,
         Cast (Process));
--  diff
   end Iterate;

   procedure Reverse_Iterate (
      Container : Set'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Binary_Trees.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
--  diff
--  diff
      Binary_Trees.Reverse_Iterate (
         Container.Root,
         Cast (Process));
--  diff
   end Reverse_Iterate;

   function Iterate (Container : Set'Class)
      return Set_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Set_Iterator'(
         First => First (Set (Container)),
         Last => Last (Set (Container)));
   end Iterate;

   function Iterate (Container : Set'Class; First, Last : Cursor)
      return Set_Iterator_Interfaces.Reversible_Iterator'Class
   is
      pragma Unreferenced (Container);
      Actual_First : Cursor := First;
      Actual_Last : Cursor := Last;
   begin
      if Actual_First = No_Element
         or else Actual_Last = No_Element
         or else Actual_Last < Actual_First
      then
         Actual_First := No_Element;
         Actual_Last := No_Element;
      end if;
      return Set_Iterator'(First => Actual_First, Last => Actual_Last);
   end Iterate;

--  diff (Adjust)
--
--
--

   overriding function First (Object : Set_Iterator) return Cursor is
   begin
      return Object.First;
   end First;

   overriding function Next (Object : Set_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.Last then
         return No_Element;
      else
         return Next (Position);
      end if;
   end Next;

   overriding function Last (Object : Set_Iterator) return Cursor is
   begin
      return Object.Last;
   end Last;

   overriding function Previous (Object : Set_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.First then
         return No_Element;
      else
         return Previous (Position);
      end if;
   end Previous;

   package body Generic_Keys is

      type Context_Type is limited record
         Left : not null access Key_Type;
      end record;
      pragma Suppress_Initialization (Context_Type);

      function Compare_Key (
         Position : not null Binary_Trees.Node_Access;
         Params : System.Address)
         return Integer;
      function Compare_Key (
         Position : not null Binary_Trees.Node_Access;
         Params : System.Address)
         return Integer
      is
         Context : Context_Type;
         for Context'Address use Params;
         Left : Key_Type
            renames Context.Left.all;
         Right : Key_Type
            renames Key (Downcast (Position).Element.all);
      begin
         --  [gcc-4.9] same as above
         if Left < Right then
            return -1;
         elsif Right < Left then
            return 1;
         else
            return 0;
         end if;
      end Compare_Key;

      function Equivalent_Keys (Left, Right : Key_Type) return Boolean is
      begin
         return not (Left < Right) and then not (Right < Left);
      end Equivalent_Keys;

      function Key (Position : Cursor) return Key_Type is
      begin
         return Key (Position.Element.all);
      end Key;

--  diff (Element)
--
--
--

--  diff (Replace)
--
--
--
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

      procedure Delete (Container : in out Set; Key : Key_Type) is
         Position : Cursor := Find (Container, Key);
      begin
         Delete (Container, Position);
      end Delete;

      function Find (Container : Set; Key : Key_Type) return Cursor is
      begin
--  diff
--  diff
--  diff
--  diff
            declare
               Context : Context_Type := (Left => Key'Unrestricted_Access);
            begin
               return Downcast (Binary_Trees.Find (
                  Container.Root,
                  Binary_Trees.Just,
                  Context'Address,
                  Compare => Compare_Key'Access));
            end;
--  diff
      end Find;

      function Floor (Container : Set; Key : Key_Type) return Cursor is
      begin
--  diff
--  diff
--  diff
--  diff
            declare
               Context : Context_Type := (Left => Key'Unrestricted_Access);
            begin
               return Downcast (Binary_Trees.Find (
                  Container.Root,
                  Binary_Trees.Floor,
                  Context'Address,
                  Compare => Compare_Key'Access));
            end;
--  diff
      end Floor;

      function Ceiling (Container : Set; Key : Key_Type) return Cursor is
      begin
--  diff
--  diff
--  diff
--  diff
            declare
               Context : Context_Type := (Left => Key'Unrestricted_Access);
            begin
               return Downcast (Binary_Trees.Find (
                  Container.Root,
                  Binary_Trees.Ceiling,
                  Context'Address,
                  Compare => Compare_Key'Access));
            end;
--  diff
      end Ceiling;

      function Contains (Container : Set; Key : Key_Type) return Boolean is
      begin
         return Find (Container, Key) /= null;
      end Contains;

      procedure Update_Element_Preserving_Key (
         Container : in out Set;
         Position : Cursor;
         Process : not null access procedure (
            Element : in out Element_Type)) is
      begin
         Process (
            Reference_Preserving_Key (
               Container,
               Position).Element.all);
      end Update_Element_Preserving_Key;

      function Reference_Preserving_Key (
         Container : aliased in out Set;
         Position : Cursor)
         return Reference_Type
      is
         pragma Unreferenced (Container);
      begin
         return (Element => Position.Element.all'Access);
      end Reference_Preserving_Key;

      function Constant_Reference (
         Container : aliased Set;
         Key : Key_Type)
         return Constant_Reference_Type is
      begin
         return Constant_Reference (Container, Find (Container, Key));
      end Constant_Reference;

      function Reference_Preserving_Key (
         Container : aliased in out Set;
         Key : Key_Type)
         return Reference_Type is
      begin
         return Reference_Preserving_Key (Container, Find (Container, Key));
      end Reference_Preserving_Key;

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
         return Left.Length = Right.Length
            and then Binary_Trees.Equivalent (
               Left.Root,
               Right.Root,
               Equivalent'Access);
      end "=";

   end Equivalents;

end Ada.Containers.Limited_Ordered_Sets;
