pragma License (Unrestricted);
--  Ada 2005
with Ada.Iterator_Interfaces;
private with Ada.Containers.Binary_Trees;
private with Ada.Containers.Binary_Trees.Arne_Andersson;
private with Ada.Containers.Copy_On_Write;
private with Ada.Finalization;
private with Ada.Streams;
generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Ordered_Maps is
   pragma Preelaborate;
   pragma Remote_Types;

   function Equivalent_Keys (Left, Right : Key_Type) return Boolean;

   type Map is tagged private
      with
         Constant_Indexing => Constant_Reference,
         Variable_Indexing => Reference,
         Default_Iterator => Iterate,
         Iterator_Element => Element_Type;
   pragma Preelaborable_Initialization (Map);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   --  modified
--  Empty_Map : constant Map;
   function Empty_Map return Map;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   package Map_Iterator_Interfaces is
      new Iterator_Interfaces (Cursor, Has_Element);

   overriding function "=" (Left, Right : Map) return Boolean;

   function Length (Container : Map) return Count_Type;

   function Is_Empty (Container : Map) return Boolean;

   procedure Clear (Container : in out Map);

   function Key (Position : Cursor) return Key_Type;
--  diff
--  diff
--  diff

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
      Element : not null access constant Element_Type) is private
      with Implicit_Dereference => Element;

   type Reference_Type (
      Element : not null access Element_Type) is private
      with Implicit_Dereference => Element;

   function Constant_Reference (
      Container : aliased Map;
      Position : Cursor)
      return Constant_Reference_Type;

   function Reference (
      Container : aliased in out Map;
      Position : Cursor)
      return Reference_Type;

   function Constant_Reference (
      Container : aliased Map;
      Key : Key_Type)
      return Constant_Reference_Type;

   function Reference (
      Container : aliased in out Map;
      Key : Key_Type)
      return Reference_Type;

   procedure Assign (Target : in out Map; Source : Map);

   function Copy (Source : Map) return Map;

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

--  procedure Delete_First (Container : in out Map);

--  procedure Delete_Last (Container : in out Map);

   function First (Container : Map) return Cursor;

--  function First_Element (Container : Map) return Element_Type;

--  function First_Key (Container : Map) return Key_Type;

   function Last (Container : Map) return Cursor;

--  function Last_Element (Container : Map) return Element_Type;

--  function Last_Key (Container : Map) return Key_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   function Find (Container : Map; Key : Key_Type) return Cursor;

   --  modified
   function Element (
      Container : Map'Class; -- not primitive
      Key : Key_Type)
      return Element_Type;

   function Floor (Container : Map; Key : Key_Type) return Cursor;

   function Ceiling (Container : Map; Key : Key_Type) return Cursor;

   function Contains (Container : Map; Key : Key_Type) return Boolean;

   function "<" (Left, Right : Cursor) return Boolean;

--  function ">" (Left, Right : Cursor) return Boolean;

   function "<" (Left : Cursor; Right : Key_Type) return Boolean;

--  function ">" (Left : Cursor; Right : Key_Type) return Boolean;

--  function "<" (Left : Key_Type; Right : Cursor) return Boolean;

--  function ">" (Left : Key_Type; Right : Cursor) return Boolean;

   --  modified
   procedure Iterate (
      Container : Map'Class; -- not primitive
      Process : not null access procedure (Position : Cursor));

   --  modified
   procedure Reverse_Iterate (
      Container : Map'Class; -- not primitive
      Process : not null access procedure (Position : Cursor));

   function Iterate (Container : Map)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class;

--  diff (Equivalents)
--
--
--
--

private

   package Base renames Binary_Trees.Arne_Andersson;

   type Key_Access is access Key_Type;
   type Element_Access is access Element_Type;

   type Node is limited record
      Super : aliased Base.Node;
      Key : Key_Access;
      Element : Element_Access;
   end record;

   --  place Super at first whether Element_Type is controlled-type
   for Node use record
      Super at 0 range 0 .. Base.Node_Size - 1;
   end record;

   type Data is limited record
      Super : aliased Copy_On_Write.Data;
      Root : Binary_Trees.Node_Access := null;
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

   type Cursor is access Node;

--  diff (Key_Reference_Type)
--  diff

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is null record;

   type Reference_Type (
      Element : not null access Element_Type) is null record;

   type Map_Access is access constant Map;
   for Map_Access'Storage_Size use 0;

   type Map_Iterator is
      new Map_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : not null Map_Access;
   end record;

   overriding function First (Object : Map_Iterator) return Cursor;
   overriding function Next (Object : Map_Iterator; Position : Cursor)
      return Cursor;
   overriding function Last (Object : Map_Iterator) return Cursor;
   overriding function Previous (Object : Map_Iterator; Position : Cursor)
      return Cursor;

   package Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Map);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Map);

      procedure Missing_Read (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : out Cursor);
      procedure Missing_Write (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : Cursor);

--  diff (Read)
--
--
--  diff (Write)
--
--

      procedure Missing_Read (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : out Constant_Reference_Type);
      procedure Missing_Write (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : Constant_Reference_Type);

      procedure Missing_Read (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : out Reference_Type);
      procedure Missing_Write (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : Reference_Type);

      procedure Missing_Read (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : out Map_Iterator);
      function Missing_Input (
         Stream : not null access Streams.Root_Stream_Type'Class)
         return Map_Iterator;
      procedure Missing_Write (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : Map_Iterator);

      pragma Import (Ada, Missing_Read, "__drake_program_error");
      pragma Import (Ada, Missing_Input, "__drake_program_error");
      pragma Import (Ada, Missing_Write, "__drake_program_error");

   end Streaming;

   for Map'Read use Streaming.Read;
   for Map'Write use Streaming.Write;

   for Cursor'Read use Streaming.Missing_Read;
   for Cursor'Write use Streaming.Missing_Write;

--  diff ('Read)
--  diff ('Write)

   for Constant_Reference_Type'Read use Streaming.Missing_Read;
   for Constant_Reference_Type'Write use Streaming.Missing_Write;

   for Reference_Type'Read use Streaming.Missing_Read;
   for Reference_Type'Write use Streaming.Missing_Write;

   for Map_Iterator'Read use Streaming.Missing_Read;
   for Map_Iterator'Input use Streaming.Missing_Input;
   for Map_Iterator'Write use Streaming.Missing_Write;
   for Map_Iterator'Output use Streaming.Missing_Write;

   No_Element : constant Cursor := null;

end Ada.Containers.Indefinite_Ordered_Maps;
