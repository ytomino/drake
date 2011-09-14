pragma License (Unrestricted);
--  Ada 2005
private with Ada.Containers.Inside.Copy_On_Write;
private with Ada.Containers.Inside.Hash_Tables;
private with Ada.Finalization;
private with Ada.Streams;
generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Hashed_Maps is
   pragma Preelaborate;
--  pragma Remote_Types; -- [gcc 4.5/4.6] it defends to define Reference_Type

   type Map is tagged private;
   pragma Preelaborable_Initialization (Map);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   --  modified
--  Empty_Map : constant Map;
   function Empty_Map return Map;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

--  package Map_Iterator_Interfaces is new
--    Ada.Iterator_Interfaces (Cursor, Has_Element);
   type Iterator is limited private;
   function First (Object : Iterator) return Cursor;
   function Next (Object : Iterator; Position : Cursor) return Cursor;

   function "=" (Left, Right : Map) return Boolean;

   function Capacity (Container : Map) return Count_Type;

   procedure Reserve_Capacity (Container : in out Map; Capacity : Count_Type);

   function Length (Container : Map) return Count_Type;

   function Is_Empty (Container : Map) return Boolean;

   procedure Clear (Container : in out Map);

   function Key (Position : Cursor) return Key_Type;

   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element (
      Container : in out Map;
      Position : Cursor;
      New_Item : Element_Type);

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : Element_Type));

   --  modified
   procedure Update_Element (
      Container : in out Map'Class; -- not primitive
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : in out Element_Type));

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is private;

   type Reference_Type (
      Element : not null access Element_Type) is private;

   function Constant_Reference (
      Container : not null access constant Map; -- [gcc 4.5/4.6] aliased
      Position : Cursor)
      return Constant_Reference_Type;

   function Reference (
      Container : not null access Map; -- [gcc 4.5/4.6] aliased
      Position : Cursor)
      return Reference_Type;

   function Constant_Reference (
      Container : not null access constant Map; -- [gcc 4.5/4.6] aliased
      Key : Key_Type)
      return Constant_Reference_Type;

   function Reference (
      Container : not null access Map; -- [gcc 4.5/4.6] aliased
      Key : Key_Type)
      return Reference_Type;

   procedure Assign (Target : in out Map; Source : Map);

   function Copy (Source : Map; Capacity : Count_Type := 0) return Map;

   procedure Move (Target : in out Map; Source : in out Map);

   procedure Insert (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type;
      Position : out Cursor;
      Inserted : out Boolean);

--  diff (Insert)
--
--
--
--

   procedure Insert (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type);

   procedure Include (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type);

   procedure Replace (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type);

   procedure Exclude (Container : in out Map; Key : Key_Type);

   procedure Delete (Container : in out Map; Key : Key_Type);

   procedure Delete (Container : in out Map; Position : in out Cursor);

   function First (Container : Map) return Cursor;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Find (Container : Map; Key : Key_Type) return Cursor;

   --  modified
   function Element (
      Container : Map'Class; -- not primitive
      Key : Key_Type) return Element_Type;

   function Contains (Container : Map; Key : Key_Type) return Boolean;

   function Equivalent_Keys (Left, Right : Cursor) return Boolean;

   function Equivalent_Keys (Left : Cursor; Right : Key_Type) return Boolean;

--  function Equivalent_Keys (Left : Key_Type; Right : Cursor) return Boolean;

   --  modified
   procedure Iterate (
      Container : Map'Class; -- not primitive
      Process : not null access procedure (Position : Cursor));

--  function Iterate (Container : Map)
--    return Map_Iterator_Interfaces.Forward_Iterator'Class;
   function Iterate (Container : Map)
      return Iterator;

--  diff (Equivalent)
--
--
--
--

private

   package Hash_Tables renames Containers.Inside.Hash_Tables;
   package Copy_On_Write renames Containers.Inside.Copy_On_Write;

   type Key_Access is access Key_Type;
   type Element_Access is access Element_Type;

   type Node is limited record
      Super : aliased Hash_Tables.Node;
      Key : Key_Access;
      Element : Element_Access;
   end record;

   --  place Super at first whether Element_Type is controlled-type
   for Node use record
      Super at 0 range 0 .. Hash_Tables.Node_Size - 1;
   end record;

   type Data is limited record
      Super : aliased Copy_On_Write.Data;
      Table : Hash_Tables.Table_Access := null;
      Length : Count_Type := 0;
   end record;

   type Data_Access is access Data;

   type Map is new Finalization.Controlled with record
      Super : aliased Copy_On_Write.Container;
--  diff
   end record;

   overriding procedure Adjust (Object : in out Map);
   overriding procedure Finalize (Object : in out Map)
      renames Clear;

   package No_Primitives is
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : out Map);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : Map);
   end No_Primitives;

   for Map'Read use No_Primitives.Read;
   for Map'Write use No_Primitives.Write;

   type Cursor is access Node;

   No_Element : constant Cursor := null;

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is null record;

   type Reference_Type (
      Element : not null access Element_Type) is null record;

   type Iterator is not null access constant Map;

end Ada.Containers.Indefinite_Hashed_Maps;
