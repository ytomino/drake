with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Streams; -- [gcc-4.7] can not search in private with
with System;
package body Ada.Containers.Indefinite_Ordered_Sets is
   use type Binary_Trees.Node_Access;
   use type Copy_On_Write.Data_Access;

   function Upcast is
      new Unchecked_Conversion (Cursor, Binary_Trees.Node_Access);
   function Downcast is
      new Unchecked_Conversion (Binary_Trees.Node_Access, Cursor);

   function Upcast is
      new Unchecked_Conversion (Data_Access, Copy_On_Write.Data_Access);
   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Node, Cursor);

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
      Left : Element_Type
         renames Context.Left.all;
      Right : Element_Type
         renames Downcast (Position).Element.all;
   begin
      --  [gcc-4.9] outputs wrong code for combination of
      --    constrained short String used as Key_Type (ex. String (1 .. 4))
      --    and instantiation of Ada.Containers.Composites.Compare here
      if Left < Right then
         return -1;
      elsif Right < Left then
         return 1;
      else
         return 0;
      end if;
   end Compare_Element;

   function Compare_Node (Left, Right : not null Binary_Trees.Node_Access)
      return Integer;
   function Compare_Node (Left, Right : not null Binary_Trees.Node_Access)
      return Integer
   is
      Left_E : Element_Type
         renames Downcast (Left).Element.all;
      Right_E : Element_Type
         renames Downcast (Right).Element.all;
   begin
      --  [gcc-4.9] same as above
      if Left_E < Right_E then
         return -1;
      elsif Right_E < Left_E then
         return 1;
      else
         return 0;
      end if;
   end Compare_Node;

   procedure Allocate_Element (
      Item : out Element_Access;
      New_Item : Element_Type);
   procedure Allocate_Element (
      Item : out Element_Access;
      New_Item : Element_Type) is
   begin
      Item := new Element_Type'(New_Item);
   end Allocate_Element;

   procedure Allocate_Node (Item : out Cursor; New_Item : Element_Type);
   procedure Allocate_Node (Item : out Cursor; New_Item : Element_Type) is
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Cursor, Free);
      X : aliased Cursor := new Node;
   begin
      Holder.Assign (X);
      Allocate_Element (X.Element, New_Item);
      Holder.Clear;
      Item := X;
   end Allocate_Node;

   procedure Copy_Node (
      Target : out Binary_Trees.Node_Access;
      Source : not null Binary_Trees.Node_Access);
   procedure Copy_Node (
      Target : out Binary_Trees.Node_Access;
      Source : not null Binary_Trees.Node_Access)
   is
      New_Node : Cursor;
   begin
      Allocate_Node (New_Node, Downcast (Source).Element.all);
      Target := Upcast (New_Node);
   end Copy_Node;

   procedure Free_Node (Object : in out Binary_Trees.Node_Access);
   procedure Free_Node (Object : in out Binary_Trees.Node_Access) is
      X : Cursor := Downcast (Object);
   begin
      Free (X.Element);
      Free (X);
      Object := null;
   end Free_Node;

   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type);
   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Max_Length);
      pragma Unreferenced (Capacity);
      New_Data : constant Data_Access := new Data'(
         Super => <>,
         Root => null,
         Length => 0);
   begin
      Target := Upcast (New_Data);
   end Allocate_Data;

   procedure Copy_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Max_Length : Count_Type;
      Capacity : Count_Type);
   procedure Copy_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Max_Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Length);
      pragma Unreferenced (Max_Length);
      pragma Unreferenced (Capacity);
   begin
      Allocate_Data (Target, 0, 0);
      Base.Copy (
         Downcast (Target).Root,
         Downcast (Target).Length,
         Source => Downcast (Source).Root,
         Copy => Copy_Node'Access);
   end Copy_Data;

   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   procedure Free_Data (Data : in out Copy_On_Write.Data_Access);
   procedure Free_Data (Data : in out Copy_On_Write.Data_Access) is
      X : Data_Access := Downcast (Data);
   begin
      Binary_Trees.Free (
         X.Root,
         X.Length,
         Free => Free_Node'Access);
      Free (X);
      Data := null;
   end Free_Data;

   procedure Unique (Container : in out Set; To_Update : Boolean);
   procedure Unique (Container : in out Set; To_Update : Boolean) is
   begin
      if Copy_On_Write.Shared (Container.Super.Data) then
         Copy_On_Write.Unique (
            Target => Container.Super'Access,
            Target_Length => 0, -- Length is unused
            Target_Capacity => 0, -- Capacity is unused
            New_Length => 0,
            New_Capacity => 0,
            To_Update => To_Update,
            Allocate => Allocate_Data'Access,
            Move => Copy_Data'Access,
            Copy => Copy_Data'Access,
            Free => Free_Data'Access);
      end if;
   end Unique;

   --  implementation

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean is
   begin
      return not (Left < Right) and then not (Right < Left);
   end Equivalent_Elements;

   function Empty_Set return Set is
   begin
      return (Finalization.Controlled with Super => (null, null));
   end Empty_Set;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   overriding function "=" (Left, Right : Set) return Boolean is
      function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
         return Boolean;
      function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
         return Boolean is
      begin
         return Downcast (Left).Element.all = Downcast (Right).Element.all;
      end Equivalent;
   begin
      if Is_Empty (Left) then
         return Is_Empty (Right);
      elsif Left.Super.Data = Right.Super.Data then
         return True;
      elsif Length (Left) = Length (Right) then
         Unique (Left'Unrestricted_Access.all, False);
         Unique (Right'Unrestricted_Access.all, False);
         return Binary_Trees.Equivalent (
            Downcast (Left.Super.Data).Root,
            Downcast (Right.Super.Data).Root,
            Equivalent'Access);
      else
         return False;
      end if;
   end "=";

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
      if Is_Empty (Left) then
         return Is_Empty (Right);
      elsif Left.Super.Data = Right.Super.Data then
         return True;
      elsif Length (Left) = Length (Right) then
         Unique (Left'Unrestricted_Access.all, False);
         Unique (Right'Unrestricted_Access.all, False);
         return Binary_Trees.Equivalent (
            Downcast (Left.Super.Data).Root,
            Downcast (Right.Super.Data).Root,
            Equivalent'Access);
      else
         return False;
      end if;
   end Equivalent_Sets;

   function To_Set (New_Item : Element_Type) return Set is
   begin
      return Result : Set do
         Insert (Result, New_Item);
      end return;
   end To_Set;

--  diff (Generic_Array_To_Set)
--
--
--
--
--
--
--

   function Length (Container : Set) return Count_Type is
   begin
      if Container.Super.Data = null then
         return 0;
      else
         return Downcast (Container.Super.Data).Length;
      end if;
   end Length;

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Super.Data = null
         or else Downcast (Container.Super.Data).Root = null;
   end Is_Empty;

   procedure Clear (Container : in out Set) is
   begin
      Copy_On_Write.Clear (
         Container.Super'Access,
         Free => Free_Data'Access);
   end Clear;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Element.all;
   end Element;

   procedure Replace_Element (
      Container : in out Set;
      Position : Cursor;
      New_Item : Element_Type) is
   begin
      Unique (Container, True);
      Free (Position.Element);
      Allocate_Element (Position.Element, New_Item);
   end Replace_Element;

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

   procedure Assign (Target : in out Set; Source : Set) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Assign;

   function Copy (Source : Set) return Set is
   begin
      return Result : Set do
         Copy_On_Write.Copy (
            Result.Super'Access,
            Source.Super'Access,
            0, -- Length is unused
            0, -- Capacity is unused
            Allocate => Allocate_Data'Access,
            Copy => Copy_Data'Access);
      end return;
   end Copy;

   procedure Move (Target : in out Set; Source : in out Set) is
   begin
      Copy_On_Write.Move (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
--  diff
--  diff
--  diff
   end Move;

   procedure Insert (
      Container : in out Set;
      New_Item : Element_Type;
      Position : out Cursor;
      Inserted : out Boolean)
   is
--  diff
--  diff
--  diff
      Before : constant Cursor := Ceiling (Container, New_Item);
   begin
--  diff
      Inserted := Before = null or else New_Item < Before.Element.all;
      if Inserted then
         Unique (Container, True);
         Allocate_Node (Position, New_Item);
         Base.Insert (
            Downcast (Container.Super.Data).Root,
            Downcast (Container.Super.Data).Length,
            Upcast (Before),
            Upcast (Position));
      else
         Position := Before;
      end if;
   end Insert;

   procedure Insert (
      Container : in out Set;
      New_Item : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, New_Item, Position, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

   procedure Include (Container : in out Set; New_Item : Element_Type) is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, New_Item, Position, Inserted);
      if not Inserted then
         Replace_Element (Container, Position, New_Item);
      end if;
   end Include;

   procedure Replace (Container : in out Set; New_Item : Element_Type) is
   begin
      Replace_Element (Container, Find (Container, New_Item), New_Item);
   end Replace;

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
      Unique (Container, True);
      Base.Remove (
         Downcast (Container.Super.Data).Root,
         Downcast (Container.Super.Data).Length,
         Position_2);
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

   procedure Union (Target : in out Set; Source : Set) is
   begin
      if not Is_Empty (Source) then
         if Is_Empty (Target) then
            Assign (Target, Source);
         else
            Unique (Target, True);
            Binary_Trees.Merge (
               Downcast (Target.Super.Data).Root,
               Downcast (Target.Super.Data).Length,
               Downcast (Source.Super.Data).Root,
               In_Only_Left => True,
               In_Only_Right => True,
               In_Both => True,
               Compare => Compare_Node'Access,
               Copy => Copy_Node'Access,
               Insert => Base.Insert'Access,
               Remove => Base.Remove'Access,
               Free => Free_Node'Access);
         end if;
      end if;
   end Union;

   function Union (Left, Right : Set) return Set is
   begin
      return Result : Set do
         if Is_Empty (Left) then
            Assign (Result, Right);
         elsif Is_Empty (Right) then
            Assign (Result, Left);
         else
            Unique (Result, True);
            Binary_Trees.Merge (
               Downcast (Result.Super.Data).Root,
               Downcast (Result.Super.Data).Length,
               Downcast (Left.Super.Data).Root,
               Downcast (Right.Super.Data).Root,
               In_Only_Left => True,
               In_Only_Right => True,
               In_Both => True,
               Compare => Compare_Node'Access,
               Copy => Copy_Node'Access,
               Insert => Base.Insert'Access);
         end if;
      end return;
   end Union;

   procedure Intersection (Target : in out Set; Source : Set) is
   begin
      if not Is_Empty (Target) then
         if Is_Empty (Source) then
            Clear (Target);
         else
            Unique (Target, True);
            Binary_Trees.Merge (
               Downcast (Target.Super.Data).Root,
               Downcast (Target.Super.Data).Length,
               Downcast (Source.Super.Data).Root,
               In_Only_Left => False,
               In_Only_Right => False,
               In_Both => True,
               Compare => Compare_Node'Access,
               Copy => Copy_Node'Access,
               Insert => Base.Insert'Access,
               Remove => Base.Remove'Access,
               Free => Free_Node'Access);
         end if;
      end if;
   end Intersection;

   function Intersection (Left, Right : Set) return Set is
   begin
      return Result : Set do
         if Is_Empty (Left) or else Is_Empty (Right) then
            null; -- Empty_Set
         else
            Unique (Result, True);
            Binary_Trees.Merge (
               Downcast (Result.Super.Data).Root,
               Downcast (Result.Super.Data).Length,
               Downcast (Left.Super.Data).Root,
               Downcast (Right.Super.Data).Root,
               In_Only_Left => False,
               In_Only_Right => False,
               In_Both => True,
               Compare => Compare_Node'Access,
               Copy => Copy_Node'Access,
               Insert => Base.Insert'Access);
         end if;
      end return;
   end Intersection;

   procedure Difference (Target : in out Set; Source : Set) is
   begin
      if not Is_Empty (Target) and then not Is_Empty (Source) then
         Unique (Target, True);
         Binary_Trees.Merge (
            Downcast (Target.Super.Data).Root,
            Downcast (Target.Super.Data).Length,
            Downcast (Source.Super.Data).Root,
            In_Only_Left => True,
            In_Only_Right => False,
            In_Both => False,
            Compare => Compare_Node'Access,
            Copy => Copy_Node'Access,
            Insert => Base.Insert'Access,
            Remove => Base.Remove'Access,
            Free => Free_Node'Access);
      end if;
   end Difference;

   function Difference (Left, Right : Set) return Set is
   begin
      return Result : Set do
         if Is_Empty (Left) or else Is_Empty (Right) then
            Assign (Result, Left);
         else
            Unique (Result, True);
            Binary_Trees.Merge (
               Downcast (Result.Super.Data).Root,
               Downcast (Result.Super.Data).Length,
               Downcast (Left.Super.Data).Root,
               Downcast (Right.Super.Data).Root,
               In_Only_Left => True,
               In_Only_Right => False,
               In_Both => False,
               Compare => Compare_Node'Access,
               Copy => Copy_Node'Access,
               Insert => Base.Insert'Access);
         end if;
      end return;
   end Difference;

   procedure Symmetric_Difference (Target : in out Set; Source : Set) is
   begin
      if not Is_Empty (Source) then
         if Is_Empty (Target) then
            Assign (Target, Source);
         else
            Unique (Target, True);
            Binary_Trees.Merge (
               Downcast (Target.Super.Data).Root,
               Downcast (Target.Super.Data).Length,
               Downcast (Source.Super.Data).Root,
               In_Only_Left => True,
               In_Only_Right => True,
               In_Both => False,
               Compare => Compare_Node'Access,
               Copy => Copy_Node'Access,
               Insert => Base.Insert'Access,
               Remove => Base.Remove'Access,
               Free => Free_Node'Access);
         end if;
      end if;
   end Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Set) return Set is
   begin
      return Result : Set do
         if Is_Empty (Left) then
            Assign (Result, Right);
         elsif Is_Empty (Right) then
            Assign (Result, Left);
         else
            Unique (Result, True);
            Binary_Trees.Merge (
               Downcast (Result.Super.Data).Root,
               Downcast (Result.Super.Data).Length,
               Downcast (Left.Super.Data).Root,
               Downcast (Right.Super.Data).Root,
               In_Only_Left => True,
               In_Only_Right => True,
               In_Both => False,
               Compare => Compare_Node'Access,
               Copy => Copy_Node'Access,
               Insert => Base.Insert'Access);
         end if;
      end return;
   end Symmetric_Difference;

   function Overlap (Left, Right : Set) return Boolean is
   begin
      if Is_Empty (Left) or else Is_Empty (Right) then
         return False;
      else
         return Binary_Trees.Overlap (
            Downcast (Left.Super.Data).Root,
            Downcast (Right.Super.Data).Root,
            Compare => Compare_Node'Access);
      end if;
   end Overlap;

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
      if Is_Empty (Subset) then
         return True;
      elsif Is_Empty (Of_Set) then
         return False;
      else
         return Binary_Trees.Is_Subset (
            Downcast (Subset.Super.Data).Root,
            Downcast (Of_Set.Super.Data).Root,
            Compare => Compare_Node'Access);
      end if;
   end Is_Subset;

   function First (Container : Set) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Binary_Trees.First (
            Downcast (Container.Super.Data).Root));
      end if;
   end First;

   function First_Element (Container : Set'Class)
      return Element_Type is
   begin
      return Element (Last (Set (Container)));
   end First_Element;

   function Last (Container : Set) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Binary_Trees.Last (
            Downcast (Container.Super.Data).Root));
      end if;
   end Last;

   function Last_Element (Container : Set'Class)
      return Element_Type is
   begin
      return Element (Last (Set (Container)));
   end Last_Element;

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
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Binary_Trees.Find (
               Downcast (Container.Super.Data).Root,
               Binary_Trees.Just,
               Context'Address,
               Compare => Compare_Element'Access));
         end;
      end if;
   end Find;

   function Floor (Container : Set; Item : Element_Type) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Binary_Trees.Find (
               Downcast (Container.Super.Data).Root,
               Binary_Trees.Floor,
               Context'Address,
               Compare => Compare_Element'Access));
         end;
      end if;
   end Floor;

   function Ceiling (Container : Set; Item : Element_Type) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Binary_Trees.Find (
               Downcast (Container.Super.Data).Root,
               Binary_Trees.Ceiling,
               Context'Address,
               Compare => Compare_Element'Access));
         end;
      end if;
   end Ceiling;

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= null;
   end Contains;

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= Right and then Left.Element.all < Right.Element.all;
   end "<";

   function "<" (Left : Cursor; Right : Element_Type) return Boolean is
   begin
      return Left.Element.all < Right;
   end "<";

   procedure Iterate (
      Container : Set'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Binary_Trees.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
      if not Is_Empty (Set (Container)) then
         Unique (Set (Container)'Unrestricted_Access.all, False);
         Binary_Trees.Iterate (
            Downcast (Container.Super.Data).Root,
            Cast (Process));
      end if;
   end Iterate;

   procedure Reverse_Iterate (
      Container : Set'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Binary_Trees.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
      if not Is_Empty (Set (Container)) then
         Unique (Set (Container)'Unrestricted_Access.all, False);
         Binary_Trees.Reverse_Iterate (
            Downcast (Container.Super.Data).Root,
            Cast (Process));
      end if;
   end Reverse_Iterate;

   function Iterate (Container : Set)
      return Set_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Set_Iterator'(
         First => First (Container),
         Last => Last (Container));
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

   overriding procedure Adjust (Object : in out Set) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

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

      function Element (Container : Set; Key : Key_Type) return Element_Type is
      begin
         return Element (Find (Container, Key));
      end Element;

      procedure Replace (
         Container : in out Set;
         Key : Key_Type;
         New_Item : Element_Type) is
      begin
         Replace_Element (Container, Find (Container, Key), New_Item);
      end Replace;

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
         if Is_Empty (Container) then
            return null;
         else
            Unique (Container'Unrestricted_Access.all, False);
            declare
               Context : Context_Type := (Left => Key'Unrestricted_Access);
            begin
               return Downcast (Binary_Trees.Find (
                  Downcast (Container.Super.Data).Root,
                  Binary_Trees.Just,
                  Context'Address,
                  Compare => Compare_Key'Access));
            end;
         end if;
      end Find;

      function Floor (Container : Set; Key : Key_Type) return Cursor is
      begin
         if Is_Empty (Container) then
            return null;
         else
            Unique (Container'Unrestricted_Access.all, False);
            declare
               Context : Context_Type := (Left => Key'Unrestricted_Access);
            begin
               return Downcast (Binary_Trees.Find (
                  Downcast (Container.Super.Data).Root,
                  Binary_Trees.Floor,
                  Context'Address,
                  Compare => Compare_Key'Access));
            end;
         end if;
      end Floor;

      function Ceiling (Container : Set; Key : Key_Type) return Cursor is
      begin
         if Is_Empty (Container) then
            return null;
         else
            Unique (Container'Unrestricted_Access.all, False);
            declare
               Context : Context_Type := (Left => Key'Unrestricted_Access);
            begin
               return Downcast (Binary_Trees.Find (
                  Downcast (Container.Super.Data).Root,
                  Binary_Trees.Ceiling,
                  Context'Address,
                  Compare => Compare_Key'Access));
            end;
         end if;
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
         return Reference_Type is
      begin
         Unique (Container, True);
--  diff
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

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Set)
      is
         Length : Count_Type'Base;
      begin
         Count_Type'Read (Stream, Length);
         Clear (Item);
         for I in 1 .. Length loop
            declare
               New_Item : constant Element_Type := Element_Type'Input (Stream);
            begin
--  diff
               Include (Item, New_Item);
            end;
         end loop;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Set)
      is
         function To_Pointer (Value : System.Address)
            return access Streams.Root_Stream_Type'Class
            with Import, Convention => Intrinsic;
         function To_Address (Value : access Streams.Root_Stream_Type'Class)
            return System.Address
            with Import, Convention => Intrinsic;
         procedure Process (
            Position : not null Binary_Trees.Node_Access;
            Params : System.Address);
         procedure Process (
            Position : not null Binary_Trees.Node_Access;
            Params : System.Address) is
         begin
            Element_Type'Output (
               To_Pointer (Params),
               Downcast (Position).Element.all);
         end Process;
      begin
         Count_Type'Write (Stream, Item.Length);
         if Item.Length > 0 then
            Binary_Trees.Iterate (
               Downcast (Item.Super.Data).Root,
               To_Address (Stream),
               Process => Process'Access);
         end if;
      end Write;

   end Streaming;

end Ada.Containers.Indefinite_Ordered_Sets;
