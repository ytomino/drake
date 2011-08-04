with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
package body Ada.Containers.Limited_Hashed_Maps is
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
            Context : Context_Type := (Left => Key'Unrestricted_Access);
         begin
            return Downcast (Hash_Tables.Find (
               Container.Table,
               Hash,
               Context'Address,
               Equivalent => Equivalent_Key'Access));
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

   function Capacity (Container : Map) return Count_Type is
   begin
      return Hash_Tables.Capacity (Container.Table);
--  diff
--  diff
--  diff
--  diff
--  diff
   end Capacity;

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

   procedure Delete (Container : in out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in out Map; Position : in out Cursor) is
   begin
--  diff
      Hash_Tables.Remove (
         Container.Table,
         Container.Length,
         Upcast (Position));
      Free (Position);
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
      return (Finalization.Limited_Controlled with Table => null, Length => 0);
   end Empty_Map;

   function Equivalent_Keys (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Equivalent_Keys (Left.Key.all, Right);
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
      return Find (Container, Hash (Key), Key);
   end Find;

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

   function First (Object : Iterator) return Cursor is
   begin
      return First (Object.all);
   end First;

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

   procedure Insert (
      Container : in out Map;
      New_Key : not null access function (C : Map) return Key_Type;
      New_Item : not null access function (C : Map) return Element_Type;
      Position : out Cursor)
   is
      Key : Key_Access := new Key_Type'(New_Key (Container));
      New_Hash : constant Hash_Type := Hash (Key.all);
   begin
      if Find (Container, New_Hash, Key.all) = null then
         Position := new Node'(
            Super => <>,
            Key => Key,
            Element => new Element_Type'(New_Item (Container)));
         Hash_Tables.Insert (
            Container.Table,
            Container.Length,
            New_Hash,
            Upcast (Position));
      else
         Free (Key);
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
      return Container.Length = 0;
--  diff
   end Is_Empty;

   procedure Iterate (
      Container : Map;
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

   function Iterate (Container : not null access constant Map)
      return Iterator is
   begin
      return Iterator (Container);
   end Iterate;

--  diff (Key)
--
--
--

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

   procedure Reserve_Capacity (
      Container : in out Map;
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
--  diff
      Hash_Tables.Rebuild (
         Container.Table,
         New_Capacity);
   end Reserve_Capacity;

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
