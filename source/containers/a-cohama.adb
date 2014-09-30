--  diff (Ada.Exceptions.Finally)
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
package body Ada.Containers.Hashed_Maps is
   use type Hash_Tables.Table_Access;
   use type Copy_On_Write.Data_Access;

   function Upcast is
      new Unchecked_Conversion (Cursor, Hash_Tables.Node_Access);
   function Downcast is
      new Unchecked_Conversion (Hash_Tables.Node_Access, Cursor);

   function Upcast is
      new Unchecked_Conversion (Data_Access, Copy_On_Write.Data_Access);
   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

--  diff
--  diff
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
         Downcast (Position).Key);
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

   procedure Copy_Node (
      Target : out Hash_Tables.Node_Access;
      Source : not null Hash_Tables.Node_Access);
   procedure Copy_Node (
      Target : out Hash_Tables.Node_Access;
      Source : not null Hash_Tables.Node_Access)
   is
      New_Node : constant Cursor := new Node'(
         Super => <>,
         Key => Downcast (Source).Key,
         Element => Downcast (Source).Element);
   begin
      Target := Upcast (New_Node);
   end Copy_Node;

   procedure Free_Node (Object : in out Hash_Tables.Node_Access);
   procedure Free_Node (Object : in out Hash_Tables.Node_Access) is
      X : Cursor := Downcast (Object);
   begin
--  diff
--  diff
      Free (X);
      Object := null;
   end Free_Node;

   procedure Allocate_Data (
      Target : out Copy_On_Write.Data_Access;
      Capacity : Count_Type);
   procedure Allocate_Data (
      Target : out Copy_On_Write.Data_Access;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Capacity);
      New_Data : constant Data_Access := new Data'(
         Super => <>,
         Table => null,
         Length => 0);
   begin
      Target := Upcast (New_Data);
   end Allocate_Data;

   procedure Copy_Data (
      Target : out Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Capacity : Count_Type);
   procedure Copy_Data (
      Target : out Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Length);
      New_Data : constant Data_Access := new Data'(
         Super => <>,
         Table => null,
         Length => 0);
   begin
      Hash_Tables.Copy (
         New_Data.Table,
         New_Data.Length,
         Downcast (Source).Table,
         Capacity,
         Copy => Copy_Node'Access);
      Target := Upcast (New_Data);
   end Copy_Data;

   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   procedure Free_Data (Data : in out Copy_On_Write.Data_Access);
   procedure Free_Data (Data : in out Copy_On_Write.Data_Access) is
      X : Data_Access := Downcast (Data);
   begin
      Hash_Tables.Free (
         X.Table,
         X.Length,
         Free => Free_Node'Access);
      Free (X);
      Data := null;
   end Free_Data;

   procedure Unique (Container : in out Map; To_Update : Boolean);
   procedure Unique (Container : in out Map; To_Update : Boolean) is
   begin
      Copy_On_Write.Unique (
         Container.Super'Access,
         To_Update,
         0, -- Length is unused
         Capacity (Container),
         Allocate => Allocate_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Unique;

   function Find (Container : Map; Hash : Hash_Type; Key : Key_Type)
      return Cursor;
   function Find (Container : Map; Hash : Hash_Type; Key : Key_Type)
      return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Key'Unrestricted_Access);
         begin
            return Downcast (Hash_Tables.Find (
               Downcast (Container.Super.Data).Table,
               Hash,
               Context'Address,
               Equivalent => Equivalent_Key'Access));
         end;
      end if;
   end Find;

   --  implementation

   procedure Assign (Target : in out Map; Source : Map) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Assign;

   function Capacity (Container : Map) return Count_Type is
   begin
      if Container.Super.Data = null then
         return 0;
      else
         return Hash_Tables.Capacity (
            Downcast (Container.Super.Data).Table);
      end if;
   end Capacity;

   procedure Clear (Container : in out Map) is
   begin
      Copy_On_Write.Clear (
         Container.Super'Access,
         Free => Free_Data'Access);
   end Clear;

   function Constant_Reference (
      Container : aliased Map;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element'Access);
   end Constant_Reference;

   function Constant_Reference (
      Container : aliased Map;
      Key : Key_Type)
      return Constant_Reference_Type
   is
      Position : constant not null Cursor := Find (Container, Key);
   begin
      return (Element => Position.Element'Access);
   end Constant_Reference;

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= null;
   end Contains;

   function Copy (Source : Map; Capacity : Count_Type := 0) return Map is
   begin
      return (Finalization.Controlled with Super => Copy_On_Write.Copy (
         Source.Super'Access,
         0, -- Length is unused
         Count_Type'Max (Capacity, Length (Source)),
         Allocate => Allocate_Data'Access,
         Copy => Copy_Data'Access));
   end Copy;

   procedure Delete (Container : in out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in out Map; Position : in out Cursor) is
   begin
      Unique (Container, True);
      Hash_Tables.Remove (
         Downcast (Container.Super.Data).Table,
         Downcast (Container.Super.Data).Length,
         Upcast (Position));
      Free (Position);
   end Delete;

   function Element (
      Container : Map'Class;
      Key : Key_Type)
      return Element_Type is
   begin
      return Find (Container, Key).Element;
   end Element;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Element;
   end Element;

   function Empty_Map return Map is
   begin
      return (Finalization.Controlled with Super => (null, null));
   end Empty_Map;

   function Equivalent_Keys (Left, Right : Cursor) return Boolean is
   begin
      return Equivalent_Keys (Left.Key, Right.Key);
   end Equivalent_Keys;

   function Equivalent_Keys (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Equivalent_Keys (Left.Key, Right);
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
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Hash_Tables.First (
            Downcast (Container.Super.Data).Table));
      end if;
   end First;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
   end Has_Element;

   procedure Include (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, Key, New_Item, Position, Inserted);
      if not Inserted then
         Replace_Element (Container, Position, New_Item);
      end if;
   end Include;

   procedure Insert (
      Container : in out Map;
      Key : Key_Type;
      Position : out Cursor;
      Inserted : out Boolean)
   is
      New_Hash : constant Hash_Type := Hash (Key);
   begin
      Position := Find (Container, New_Hash, Key);
      Inserted := Position = null;
      if Inserted then
         Unique (Container, True);
         Position := new Node'(
            Super => <>,
            Key => Key,
            Element => <>);
         Hash_Tables.Insert (
            Downcast (Container.Super.Data).Table,
            Downcast (Container.Super.Data).Length,
            New_Hash,
            Upcast (Position));
      end if;
   end Insert;

   procedure Insert (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type;
      Position : out Cursor;
      Inserted : out Boolean)
   is
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
--  diff
--  diff
--  diff
      New_Hash : constant Hash_Type := Hash (Key);
   begin
--  diff
--  diff
      Position := Find (Container, New_Hash, Key);
      Inserted := Position = null;
      if Inserted then
         Unique (Container, True);
         Position := new Node'(
            Super => <>,
            Key => Key,
            Element => New_Item);
         Hash_Tables.Insert (
            Downcast (Container.Super.Data).Table,
            Downcast (Container.Super.Data).Length,
            New_Hash,
            Upcast (Position));
      end if;
   end Insert;

   procedure Insert (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type)
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
      return Container.Super.Data = null
         or else Downcast (Container.Super.Data).Length = 0;
   end Is_Empty;

   procedure Iterate (
      Container : Map'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Hash_Tables.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
      if not Is_Empty (Container) then
         Unique (Map (Container'Unrestricted_Access.all), False);
         Hash_Tables.Iterate (
            Downcast (Container.Super.Data).Table,
            Cast (Process));
      end if;
   end Iterate;

   function Iterate (Container : Map)
      return Map_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Map_Iterator'(Container => Container'Unrestricted_Access);
   end Iterate;

   function Key (Position : Cursor) return Key_Type is
   begin
      return Position.Key;
   end Key;

   function Length (Container : Map) return Count_Type is
   begin
      if Container.Super.Data = null then
         return 0;
      else
         return Downcast (Container.Super.Data).Length;
      end if;
   end Length;

   procedure Move (Target : in out Map; Source : in out Map) is
   begin
      Copy_On_Write.Move (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
--  diff
   end Move;

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Next);
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Next);
   end Next;

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : Element_Type)) is
   begin
      Process (Position.Key, Position.Element);
   end Query_Element;

   function Reference (
      Container : aliased in out Map;
      Position : Cursor)
      return Reference_Type is
   begin
      Unique (Container, True);
--  diff
      return (Element => Position.Element'Access);
   end Reference;

   function Reference (
      Container : aliased in out Map;
      Key : Key_Type)
      return Reference_Type
   is
      Position : constant not null Cursor := Find (Container, Key);
   begin
      Unique (Container, True);
      return (Element => Position.Element'Access);
   end Reference;

   procedure Replace (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type) is
   begin
      Replace_Element (Container, Find (Container, Key), New_Item);
   end Replace;

   procedure Replace_Element (
      Container : in out Map;
      Position : Cursor;
      New_Item : Element_Type) is
   begin
      Unique (Container, True);
--  diff
      Position.Element := New_Item;
   end Replace_Element;

   procedure Reserve_Capacity (
      Container : in out Map;
      Capacity : Count_Type)
   is
      New_Capacity : constant Count_Type :=
         Count_Type'Max (Capacity, Length (Container));
   begin
      Copy_On_Write.Unique (
         Container.Super'Access,
         True,
         0, -- Length is unused
         New_Capacity,
         Allocate => Allocate_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
      Hash_Tables.Rebuild (
         Downcast (Container.Super.Data).Table,
         New_Capacity);
   end Reserve_Capacity;

   procedure Update_Element (
      Container : in out Map'Class;
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : in out Element_Type)) is
   begin
      Process (
         Position.Key,
         Container.Reference (Position).Element.all);
   end Update_Element;

   overriding function "=" (Left, Right : Map) return Boolean is
      function Equivalent (Left, Right : not null Hash_Tables.Node_Access)
         return Boolean;
      function Equivalent (Left, Right : not null Hash_Tables.Node_Access)
         return Boolean is
      begin
         return Equivalent_Keys (
            Downcast (Left).Key,
            Downcast (Right).Key)
            and then Downcast (Left).Element =
               Downcast (Right).Element;
      end Equivalent;
   begin
      if Is_Empty (Left) then
         return Is_Empty (Right);
      elsif Is_Empty (Right) then
         return False;
      elsif Left.Super.Data = Right.Super.Data then
         return True;
      else
         return Hash_Tables.Equivalent (
            Downcast (Left.Super.Data).Table,
            Downcast (Left.Super.Data).Length,
            Downcast (Right.Super.Data).Table,
            Downcast (Right.Super.Data).Length,
            Equivalent => Equivalent'Access);
      end if;
   end "=";

   overriding procedure Adjust (Object : in out Map) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

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

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Map)
      is
         Length : Count_Type'Base;
      begin
         Count_Type'Read (Stream, Length);
         Clear (Item);
         for I in 1 .. Length loop
            declare
               New_Key : Key_Type;
               Position : Cursor;
               Inserted : Boolean;
               pragma Unreferenced (Inserted);
            begin
               Key_Type'Read (Stream, New_Key);
               Insert (Item, New_Key, Position, Inserted);
               Element_Type'Read (Stream, Position.Element);
            end;
         end loop;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Map)
      is
         Position : Cursor;
      begin
         Count_Type'Write (Stream, Item.Length);
         Position := First (Item);
         while Position /= null loop
            Key_Type'Write (Stream, Position.Key);
            Element_Type'Write (Stream, Position.Element);
            Next (Position);
         end loop;
      end Write;

   end Streaming;

end Ada.Containers.Hashed_Maps;
