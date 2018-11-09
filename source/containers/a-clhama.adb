with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
package body Ada.Containers.Limited_Hashed_Maps is
   use type Hash_Tables.Table_Access;
--  diff

   function Upcast is
      new Unchecked_Conversion (Cursor, Hash_Tables.Node_Access);
   function Downcast is
      new Unchecked_Conversion (Hash_Tables.Node_Access, Cursor);

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
         Downcast (Position).Key.all);
   end Equivalent_Key;

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

   procedure Free_Node (Object : in out Hash_Tables.Node_Access);
   procedure Free_Node (Object : in out Hash_Tables.Node_Access) is
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
--
--

--  diff (Free)

   procedure Free_Data (Data : in out Map);
   procedure Free_Data (Data : in out Map) is
--  diff
   begin
      Hash_Tables.Free (Data.Table, Data.Length, Free => Free_Node'Access);
--  diff
--  diff
   end Free_Data;

   procedure Reallocate (Container : in out Map; Capacity : Count_Type);
--  diff
--  diff
--  diff
   procedure Reallocate (Container : in out Map; Capacity : Count_Type) is
--  diff
--  diff
--  diff
   begin
      Hash_Tables.Rebuild (Container.Table, Capacity);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   end Reallocate;

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

   function Find (Container : Map; Hash : Hash_Type; Key : Key_Type)
      return Cursor;
   function Find (Container : Map; Hash : Hash_Type; Key : Key_Type)
      return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
--  diff
         declare
            Context : aliased Context_Type :=
               (Left => Key'Unrestricted_Access);
         begin
            return Downcast (Hash_Tables.Find (
               Container.Table,
               Hash,
               Context'Address,
               Equivalent => Equivalent_Key'Access));
         end;
      end if;
   end Find;

   --  implementation

   function Empty_Map return Map is
   begin
      return (Finalization.Limited_Controlled with Table => null, Length => 0);
   end Empty_Map;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
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
--
--
--
--
--
--
--

   function Capacity (Container : Map) return Count_Type is
--  diff
   begin
--  diff
--  diff
--  diff
      return Hash_Tables.Capacity (Container.Table);
--  diff
   end Capacity;

   procedure Reserve_Capacity (
      Container : in out Map;
      Capacity : Count_Type)
   is
      New_Capacity : constant Count_Type :=
         Count_Type'Max (Capacity, Length (Container));
   begin
      Reallocate (Container, New_Capacity);
   end Reserve_Capacity;

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
      return Container.Length = 0;
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
--
--

   procedure Move (Target : in out Map; Source : in out Map) is
   begin
      Clear (Target);
      Target.Table := Source.Table;
      Target.Length := Source.Length;
      Source.Table := null;
      Source.Length := 0;
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
      New_Hash : Hash_Type;
   begin
      Holder.Assign (New_Pair);
      New_Hash := Hash (New_Pair.Key.all);
      Position := Find (Map (Container), New_Hash, New_Pair.Key.all);
      Inserted := Position = null;
      if Inserted then
         New_Pair.Node := new Node;
         New_Pair.Node.Key := New_Pair.Key;
         New_Pair.Node.Element := new Element_Type'(New_Item.all);
         Holder.Clear;
         Position := New_Pair.Node;
         Hash_Tables.Insert (
            Container.Table,
            Container.Length,
            New_Hash,
            Upcast (Position));
--  diff
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
      Position_2 : Hash_Tables.Node_Access := Upcast (Position);
   begin
--  diff
--  diff
--  diff
--  diff
      Hash_Tables.Remove (Container.Table, Container.Length, Position_2);
--  diff
      Free_Node (Position_2);
      Position := null;
   end Delete;

   function First (Container : Map) return Cursor is
   begin
      return Downcast (Hash_Tables.First (Container.Table));
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   end First;

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Next);
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Next);
   end Next;

   function Find (Container : Map; Key : Key_Type) return Cursor is
   begin
      return Find (Container, Hash (Key), Key);
   end Find;

--  diff (Element)
--
--
--
--
--
--

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= null;
   end Contains;

   function Equivalent_Keys (Left, Right : Cursor) return Boolean is
   begin
      return Equivalent_Keys (Left.Key.all, Right.Key.all);
   end Equivalent_Keys;

   function Equivalent_Keys (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Equivalent_Keys (Left.Key.all, Right);
   end Equivalent_Keys;

   function Equivalent_Keys (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Equivalent_Keys (Left, Right.Key.all);
   end Equivalent_Keys;

   procedure Iterate (
      Container : Map'Class;
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

   function Iterate (Container : Map'Class)
      return Map_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Map_Iterator'(First => First (Map (Container)));
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
      return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return Next (Position);
   end Next;

   package body Equivalent is

      function "=" (Left, Right : Map) return Boolean is
         function Equivalent (Left, Right : not null Hash_Tables.Node_Access)
            return Boolean;
         function Equivalent (Left, Right : not null Hash_Tables.Node_Access)
            return Boolean is
         begin
            return Equivalent_Keys (
                  Downcast (Left).Key.all,
                  Downcast (Right).Key.all)
               and then Downcast (Left).Element.all =
                  Downcast (Right).Element.all;
         end Equivalent;
      begin
         return Hash_Tables.Equivalent (
            Left.Table,
            Left.Length,
            Right.Table,
            Right.Length,
            Equivalent => Equivalent'Access);
      end "=";

   end Equivalent;

end Ada.Containers.Limited_Hashed_Maps;
