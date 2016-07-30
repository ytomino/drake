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

   procedure Allocate_Node (
      Item : out Cursor;
      Key : Key_Type;
      New_Item : Element_Type);
   procedure Allocate_Node (
      Item : out Cursor;
      Key : Key_Type;
      New_Item : Element_Type) is
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
   begin
      Item := new Node'(Super => <>, Key => Key, Element => New_Item);
--  diff
--  diff
--  diff
--  diff
   end Allocate_Node;

   procedure Copy_Node (
      Target : out Hash_Tables.Node_Access;
      Source : not null Hash_Tables.Node_Access);
   procedure Copy_Node (
      Target : out Hash_Tables.Node_Access;
      Source : not null Hash_Tables.Node_Access)
   is
      Source_Node : constant Cursor := Downcast (Source);
      New_Node : Cursor;
   begin
      Allocate_Node (New_Node, Source_Node.Key, Source_Node.Element);
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
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type);
   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Max_Length);
      New_Data : constant Data_Access :=
         new Data'(Super => <>, Table => null, Length => 0);
   begin
      Hash_Tables.Rebuild (New_Data.Table, Capacity);
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
      New_Data : constant Data_Access :=
         new Data'(Super => <>, Table => null, Length => 0);
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

   procedure Reallocate (
      Container : in out Map;
      Capacity : Count_Type;
      To_Update : Boolean);
   procedure Reallocate (
      Container : in out Map;
      Capacity : Count_Type;
      To_Update : Boolean) is
   begin
      Copy_On_Write.Unique (
         Target => Container.Super'Access,
         Target_Length => 0, -- Length is unused
         Target_Capacity => Hashed_Maps.Capacity (Container),
         New_Length => 0,
         New_Capacity => Capacity,
         To_Update => To_Update,
         Allocate => Allocate_Data'Access,
         Move => Copy_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Reallocate;

   procedure Unique (Container : in out Map; To_Update : Boolean);
   procedure Unique (Container : in out Map; To_Update : Boolean) is
   begin
      if Copy_On_Write.Shared (Container.Super.Data) then
         Reallocate (
            Container,
            Capacity (Container), -- not shrinking
            To_Update);
      end if;
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

   function Empty_Map return Map is
   begin
      return (Finalization.Controlled with Super => (null, null));
   end Empty_Map;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
   end Has_Element;

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
      Left_Length : constant Count_Type := Length (Left);
      Right_Length : constant Count_Type := Length (Right);
   begin
      if Left_Length /= Right_Length then
         return False;
      elsif Left_Length = 0 or else Left.Super.Data = Right.Super.Data then
         return True;
      else
         Unique (Left'Unrestricted_Access.all, False);
         Unique (Right'Unrestricted_Access.all, False);
         declare
            Left_Data : constant Data_Access := Downcast (Left.Super.Data);
            Right_Data : constant Data_Access := Downcast (Right.Super.Data);
         begin
            return Hash_Tables.Equivalent (
               Left_Data.Table,
               Left_Data.Length,
               Right_Data.Table,
               Right_Data.Length,
               Equivalent => Equivalent'Access);
         end;
      end if;
   end "=";

   function Capacity (Container : Map) return Count_Type is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      if Data = null then
         return 0;
      else
         return Hash_Tables.Capacity (Data.Table);
      end if;
   end Capacity;

   procedure Reserve_Capacity (
      Container : in out Map;
      Capacity : Count_Type)
   is
      New_Capacity : constant Count_Type :=
         Count_Type'Max (Capacity, Length (Container));
   begin
      Reallocate (Container, New_Capacity, True);
   end Reserve_Capacity;

   function Length (Container : Map) return Count_Type is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      if Data = null then
         return 0;
      else
         return Data.Length;
      end if;
   end Length;

   function Is_Empty (Container : Map) return Boolean is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      return Data = null or else Data.Length = 0;
   end Is_Empty;

   procedure Clear (Container : in out Map) is
   begin
      Copy_On_Write.Clear (
         Container.Super'Access,
         Free => Free_Data'Access);
   end Clear;

   function Key (Position : Cursor) return Key_Type is
   begin
      return Position.Key;
   end Key;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Element;
   end Element;

   procedure Replace_Element (
      Container : in out Map;
      Position : Cursor;
      New_Item : Element_Type) is
   begin
      Unique (Container, True);
--  diff
      Position.Element := New_Item;
   end Replace_Element;

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : Element_Type)) is
   begin
      Process (Position.Key, Position.Element);
   end Query_Element;

   procedure Update_Element (
      Container : in out Map'Class;
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : in out Element_Type)) is
   begin
      Process (
         Position.Key,
         Reference (Map (Container), Position).Element.all);
   end Update_Element;

   function Constant_Reference (
      Container : aliased Map;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.all.Element'Access); -- [gcc-6] .all
   end Constant_Reference;

   function Reference (
      Container : aliased in out Map;
      Position : Cursor)
      return Reference_Type is
   begin
      Unique (Container, True);
--  diff
      return (Element => Position.all.Element'Access); -- [gcc-6] .all
   end Reference;

   function Constant_Reference (
      Container : aliased Map;
      Key : Key_Type)
      return Constant_Reference_Type is
   begin
      return Constant_Reference (Container, Find (Container, Key));
   end Constant_Reference;

   function Reference (
      Container : aliased in out Map;
      Key : Key_Type)
      return Reference_Type is
   begin
      return Reference (Container, Find (Container, Key));
   end Reference;

   procedure Assign (Target : in out Map; Source : Map) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Assign;

   function Copy (Source : Map; Capacity : Count_Type := 0) return Map is
   begin
      return Result : Map do
         Copy_On_Write.Copy (
            Result.Super'Access,
            Source.Super'Access,
            0, -- Length is unused
            Count_Type'Max (Capacity, Length (Source)),
            Allocate => Allocate_Data'Access,
            Copy => Copy_Data'Access);
      end return;
   end Copy;

   procedure Move (Target : in out Map; Source : in out Map) is
   begin
      Copy_On_Write.Move (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
--  diff
   end Move;

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
         Allocate_Node (Position, Key, New_Item);
         declare
            Data : constant Data_Access := Downcast (Container.Super.Data);
         begin
            Hash_Tables.Insert (
               Data.Table,
               Data.Length,
               New_Hash,
               Upcast (Position));
         end;
      end if;
   end Insert;

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
         Position := new Node'(Super => <>, Key => Key, Element => <>);
         declare
            Data : constant Data_Access := Downcast (Container.Super.Data);
         begin
            Hash_Tables.Insert (
               Data.Table,
               Data.Length,
               New_Hash,
               Upcast (Position));
         end;
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

   procedure Replace (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type) is
   begin
      Replace_Element (Container, Find (Container, Key), New_Item);
   end Replace;

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
      Unique (Container, True);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
      begin
         Hash_Tables.Remove (Data.Table, Data.Length, Position_2);
      end;
      Free_Node (Position_2);
      Position := null;
   end Delete;

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

   function Element (
      Container : Map'Class;
      Key : Key_Type)
      return Element_Type is
   begin
      return Element (Find (Map (Container), Key));
   end Element;

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= null;
   end Contains;

   function Equivalent_Keys (Left, Right : Cursor) return Boolean is
   begin
      return Equivalent_Keys (Left.Key, Right.Key);
   end Equivalent_Keys;

   function Equivalent_Keys (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Equivalent_Keys (Left.Key, Right);
   end Equivalent_Keys;

   function Equivalent_Keys (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Equivalent_Keys (Left, Right.Key);
   end Equivalent_Keys;

   procedure Iterate (
      Container : Map'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Hash_Tables.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
      if not Is_Empty (Map (Container)) then
         Unique (Map (Container)'Unrestricted_Access.all, False);
         Hash_Tables.Iterate (
            Downcast (Container.Super.Data).Table,
            Cast (Process));
      end if;
   end Iterate;

   function Iterate (Container : Map'Class)
      return Map_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Map_Iterator'(First => First (Map (Container)));
   end Iterate;

   overriding procedure Adjust (Object : in out Map) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

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
