with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
package body Ada.Containers.Limited_Ordered_Maps is
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

   procedure Free is new Unchecked_Deallocation (Key_Type, Key_Access);
   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Node, Cursor);

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
         renames Downcast (Position).Key.all;
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
   end Compare_Key;

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
--

   procedure Free_Node (Object : in out Binary_Trees.Node_Access);
   procedure Free_Node (Object : in out Binary_Trees.Node_Access) is
      X : Cursor := Downcast (Object);
   begin
      Free (X.Key);
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

--  diff (Free)

   procedure Free_Data (Data : in out Map);
   procedure Free_Data (Data : in out Map) is
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

   --  implementation

--  diff (Assign)
--
--
--
--
--
--

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
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

   procedure Clear (Container : in out Map) is
   begin
      Free_Data (Container);
--  diff
--  diff
   end Clear;

   function Constant_Reference (
      Container : aliased Map;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Constant_Reference;

   function Constant_Reference (
      Container : aliased Map;
      Key : Key_Type)
      return Constant_Reference_Type
   is
      Position : constant not null Cursor := Find (Container, Key);
   begin
      return (Element => Position.Element.all'Access);
   end Constant_Reference;

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= null;
   end Contains;

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

   procedure Delete (Container : in out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in out Map; Position : in out Cursor) is
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

--  diff (Element)
--
--
--

--  diff (Element)
--
--
--
--
--
--

   function Empty_Map return Map is
   begin
      return (Finalization.Limited_Controlled with Root => null, Length => 0);
   end Empty_Map;

   function Equivalent_Keys (Left, Right : Key_Type) return Boolean is
   begin
      return not (Left < Right) and then not (Right < Left);
   end Equivalent_Keys;

   procedure Exclude (Container : in out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      if Position /= null then
         Delete (Container, Position);
      end if;
   end Exclude;

   function Find (Container : Map; Key : Key_Type) return Cursor is
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

   function First (Container : Map) return Cursor is
   begin
--  diff
--  diff
--  diff
--  diff
      return Downcast (Binary_Trees.First (
         Container.Root));
--  diff
   end First;

   function Floor (Container : Map; Key : Key_Type) return Cursor is
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
--
--
--
--

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
--
--
--
--
--
--
--
--

   procedure Insert (
      Container : in out Map;
      New_Key : not null access function return Key_Type;
      New_Item : not null access function return Element_Type;
      Position : out Cursor;
      Inserted : out Boolean)
   is
      type Pair is record
         Key : Key_Access;
         Node : Cursor;
      end record;
      pragma Suppress_Initialization (Pair);
      procedure Finally (X : not null access Pair);
      procedure Finally (X : not null access Pair) is
      begin
         Free (X.Key);
         Free (X.Node);
      end Finally;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Pair, Finally);
      New_Pair : aliased Pair := (new Key_Type'(New_Key.all), null);
      Before : constant Cursor := Ceiling (Container, New_Pair.Key.all);
   begin
      Holder.Assign (New_Pair'Access);
      Inserted := Before = null or else New_Pair.Key.all < Before.Key.all;
      if Inserted then
         New_Pair.Node := new Node;
         New_Pair.Node.Key := New_Pair.Key;
         New_Pair.Node.Element := new Element_Type'(New_Item.all);
         Holder.Clear;
         Position := New_Pair.Node;
         Base.Insert (
            Container.Root,
            Container.Length,
            Upcast (Before),
            Upcast (Position));
      else
         Position := Before;
      end if;
   end Insert;

   procedure Insert (
      Container : in out Map;
      Key : not null access function return Key_Type;
      New_Item : not null access function return Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, Key, New_Item, Position, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.Root = null;
--  diff
   end Is_Empty;

   procedure Iterate (
      Container : Map'Class;
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

   function Iterate (Container : Map)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Map_Iterator'(Container => Container'Unrestricted_Access);
   end Iterate;

   function Key (Position : Cursor) return Key_Reference_Type is
   begin
      return (Element => Position.Key.all'Access);
   end Key;

   function Last (Container : Map) return Cursor is
   begin
      return Downcast (Binary_Trees.Last (Container.Root));
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   end Last;

   function Length (Container : Map) return Count_Type is
   begin
      return Container.Length;
--  diff
--  diff
--  diff
--  diff
   end Length;

   procedure Move (Target : in out Map; Source : in out Map) is
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
      Process : not null access procedure (
         Key : Key_Type;
         Element : Element_Type)) is
   begin
      Process (Position.Key.all, Position.Element.all);
   end Query_Element;

   function Reference (
      Container : aliased in out Map;
      Position : Cursor)
      return Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Reference;

   function Reference (
      Container : aliased in out Map;
      Key : Key_Type)
      return Reference_Type
   is
      Position : constant not null Cursor := Find (Container, Key);
   begin
--  diff
      return (Element => Position.Element.all'Access);
   end Reference;

--  diff (Replace)
--
--
--
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
      Container : Map'Class;
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

   procedure Update_Element (
      Container : in out Map'Class;
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : in out Element_Type)) is
   begin
      Process (
         Position.Key.all,
         Container.Reference (Position).Element.all);
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
--
--
--

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Left.Key.all < Right.Key.all;
   end "<";

   function "<" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Left.Key.all < Right;
   end "<";

--  diff (Adjust)
--
--
--

   overriding function First (Object : Map_Iterator) return Cursor is
   begin
      return First (Object.Container.all);
   end First;

   overriding function Next (Object : Map_Iterator; Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return Next (Position);
   end Next;

   overriding function Last (Object : Map_Iterator) return Cursor is
   begin
      return Last (Object.Container.all);
   end Last;

   overriding function Previous (Object : Map_Iterator; Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return Previous (Position);
   end Previous;

   package body Equivalents is

      function "=" (Left, Right : Map) return Boolean is
         function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
            return Boolean;
         function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
            return Boolean is
         begin
            return Equivalent_Keys (
               Downcast (Left).Key.all,
               Downcast (Right).Key.all)
               and then Downcast (Left).Element.all =
                  Downcast (Right).Element.all;
         end Equivalent;
      begin
         return Left.Length = Right.Length
            and then Binary_Trees.Equivalent (
               Left.Root,
               Right.Root,
               Equivalent'Access);
      end "=";

   end Equivalents;

end Ada.Containers.Limited_Ordered_Maps;
