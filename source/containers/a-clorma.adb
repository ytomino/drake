with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Comparators;
package body Ada.Containers.Limited_Ordered_Maps is
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

   function Compare is new Comparators.Generic_Compare (Key_Type);

   function Find (
      Data : Map;
      Key : Key_Type;
      Mode : Binary_Trees.Find_Mode) return Cursor;
   function Find (
      Data : Map;
      Key : Key_Type;
      Mode : Binary_Trees.Find_Mode) return Cursor
   is
      function Compare (Right : not null Binary_Trees.Node_Access)
         return Integer;
      function Compare (Right : not null Binary_Trees.Node_Access)
         return Integer is
      begin
         return Compare (Key, Downcast (Right).Key.all);
      end Compare;
   begin
      return Downcast (Binary_Trees.Find (
         Data.Root,
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
--

   procedure Free is new Unchecked_Deallocation (Key_Type, Key_Access);
   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Node, Cursor);

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

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
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

   procedure Clear (Container : in out Map) is
   begin
      Free_Data (Container);
--  diff
--  diff
   end Clear;

   function Constant_Reference (
      Container : not null access constant Map;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (
         Key => Position.Key.all'Access,
         Element => Position.Element.all'Access);
   end Constant_Reference;

   function Constant_Reference (
      Container : not null access constant Map;
      Key : Key_Type)
      return Constant_Reference_Type
   is
      Position : constant not null Cursor := Find (Container.all, Key);
   begin
      return (
         Key => Position.Key.all'Access,
         Element => Position.Element.all'Access);
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
      return Find (
         Container,
         Key,
         Binary_Trees.Just);
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
      return Find (
         Container,
         Key,
         Binary_Trees.Floor);
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
      New_Key : not null access function (C : Map) return Key_Type;
      New_Item : not null access function (C : Map) return Element_Type;
      Position : out Cursor)
--  diff
   is
      The_Key : Key_Access := new Key_Type'(New_Key (Container));
      Before : constant Cursor := Ceiling (Container, The_Key.all);
   begin
      if Before = null or else The_Key.all < Before.Key.all then
--  diff
         Position := new Node'(
            Super => <>,
            Key => The_Key,
            Element => new Element_Type'(New_Item (Container)));
         Base.Insert (
            Container.Root,
            Container.Length,
            Upcast (Before),
            Upcast (Position));
      else
         Free (The_Key);
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
--
--
--
--

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.Root = null;
--  diff
   end Is_Empty;

   procedure Iterate (
      Container : Map;
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

--  diff (Key)
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

   function No_Element return Cursor is
   begin
      return null;
   end No_Element;

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
      Container : not null access Map;
      Position : Cursor)
      return Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (
         Key => Position.Key.all'Access,
         Element => Position.Element.all'Access);
   end Reference;

   function Reference (
      Container : not null access Map;
      Key : Key_Type)
      return Reference_Type
   is
      Position : constant not null Cursor := Find (Container.all, Key);
   begin
--  diff
      return (
         Key => Position.Key.all'Access,
         Element => Position.Element.all'Access);
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
      Container : Map;
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

   procedure Update_Element (
      Container : in out Map;
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : in out Element_Type))
   is
      pragma Unreferenced (Container);
   begin
      Process (Position.Key.all, Position.Element.all);
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

   function "<=" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= null and then not (Right.Key.all < Left.Key.all);
   end "<=";

   function ">=" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= null and then not (Left.Key.all < Right.Key.all);
   end ">=";

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
