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

   function Compare_Keys (Left, Right : Key_Type) return Integer;
   function Compare_Keys (Left, Right : Key_Type) return Integer is
   begin
      if Left < Right then
         return -1;
      elsif Right < Left then
         return 1;
      else
         return 0;
      end if;
   end Compare_Keys;

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
   begin
      return Compare_Keys (Context.Left.all, Downcast (Position).Key.all);
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

   procedure Free_Data (Data : in out Map);
   procedure Free_Data (Data : in out Map) is
--  diff
   begin
      Binary_Trees.Free (Data.Root, Data.Length, Free => Free_Node'Access);
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

   --  implementation

   function Equivalent_Keys (Left, Right : Key_Type) return Boolean is
   begin
      return Compare_Keys (Left, Right) = 0;
   end Equivalent_Keys;

   function Empty_Map return Map is
   begin
      return (Finalization.Limited_Controlled with Root => null, Length => 0);
   end Empty_Map;

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
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

   function Length (Container : Map) return Count_Type is
--  diff
   begin
--  diff
--  diff
--  diff
      return Container.Length;
--  diff
   end Length;

   function Is_Empty (Container : Map) return Boolean is
--  diff
   begin
      return Container.Root = null;
   end Is_Empty;

   procedure Clear (Container : in out Map) is
   begin
      Free_Data (Container);
   end Clear;

   function Key (Position : Cursor) return Key_Reference_Type is
   begin
      return (Element => Position.Key.all'Access);
   end Key;

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
      Process : not null access procedure (
         Key : Key_Type;
         Element : Element_Type)) is
   begin
      Process (Position.Key.all, Position.Element.all);
   end Query_Element;

   procedure Update_Element (
      Container : in out Map'Class;
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : in out Element_Type)) is
   begin
      Process (
         Position.Key.all,
         Reference (Map (Container), Position).Element.all);
   end Update_Element;

   function Constant_Reference (Container : aliased Map; Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Constant_Reference;

   function Reference (Container : aliased in out Map; Position : Cursor)
      return Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Reference;

   function Constant_Reference (Container : aliased Map; Key : Key_Type)
      return Constant_Reference_Type is
   begin
      return Constant_Reference (Container, Find (Container, Key));
   end Constant_Reference;

   function Reference (Container : aliased in out Map; Key : Key_Type)
      return Reference_Type is
   begin
      return Reference (Container, Find (Container, Key));
   end Reference;

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

   procedure Insert (
      Container : in out Map'Class;
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
      procedure Finally (X : in out Pair);
      procedure Finally (X : in out Pair) is
      begin
         Free (X.Key);
         Free (X.Node);
      end Finally;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Pair, Finally);
      New_Pair : aliased Pair := (new Key_Type'(New_Key.all), null);
      Before : constant Cursor := Ceiling (Map (Container), New_Pair.Key.all);
   begin
      Holder.Assign (New_Pair);
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
--  diff
      else
         Position := Before;
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
--
--
--
--
--
--
--
--
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
      Container : in out Map'Class;
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

--  diff (Replace)
--
--
--
--
--
--

   procedure Exclude (Container : in out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      if Position /= null then
         Delete (Container, Position);
      end if;
   end Exclude;

   procedure Delete (Container : in out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in out Map; Position : in out Cursor) is
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

   procedure Delete_First (Container : in out Map'Class) is
      Position : Cursor := First (Map (Container));
   begin
      Delete (Map (Container), Position);
   end Delete_First;

   procedure Delete_Last (Container : in out Map'Class) is
      Position : Cursor := Last (Map (Container));
   begin
      Delete (Map (Container), Position);
   end Delete_Last;

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

--  diff (First_Element)
--
--
--
--

--  diff (First_Key)
--
--
--
--

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

--  diff (Last_Element)
--
--
--
--

--  diff (Last_Key)
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

--  diff (Element)
--
--
--
--
--
--

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

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= null;
   end Contains;

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= Right and then Left.Key.all < Right.Key.all;
   end "<";

   function ">" (Left, Right : Cursor) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Left.Key.all < Right;
   end "<";

   function ">" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Left < Right.Key.all;
   end "<";

   function ">" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Right < Left;
   end ">";

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

   function Iterate (Container : Map'Class)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Map_Iterator'(
         First => First (Map (Container)),
         Last => Last (Map (Container)));
   end Iterate;

   function Iterate (Container : Map'Class; First, Last : Cursor)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class
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
      return Map_Iterator'(First => Actual_First, Last => Actual_Last);
   end Iterate;

--  diff (Adjust)
--
--
--

   overriding function First (Object : Map_Iterator) return Cursor is
   begin
      return Object.First;
   end First;

   overriding function Next (Object : Map_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.Last then
         return No_Element;
      else
         return Next (Position);
      end if;
   end Next;

   overriding function Last (Object : Map_Iterator) return Cursor is
   begin
      return Object.Last;
   end Last;

   overriding function Previous (Object : Map_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.First then
         return No_Element;
      else
         return Previous (Position);
      end if;
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
