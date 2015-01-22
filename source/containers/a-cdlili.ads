pragma License (Unrestricted);
--  Ada 2005
with Ada.Iterator_Interfaces;
private with Ada.Containers.Copy_On_Write;
private with Ada.Containers.Linked_Lists;
private with Ada.Containers.Linked_Lists.Doubly;
private with Ada.Finalization;
private with Ada.Streams;
generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Doubly_Linked_Lists is
   pragma Preelaborate;
   pragma Remote_Types;

   type List is tagged private
      with
         Constant_Indexing => Constant_Reference,
         Variable_Indexing => Reference,
         Default_Iterator => Iterate,
         Iterator_Element => Element_Type;
   pragma Preelaborable_Initialization (List);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   --  modified
--  Empty_List : constant List;
   function Empty_List return List;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   package List_Iterator_Interfaces is
      new Iterator_Interfaces (Cursor, Has_Element);

   overriding function "=" (Left, Right : List) return Boolean;

   function Length (Container : List) return Count_Type;

   function Is_Empty (Container : List) return Boolean;

   procedure Clear (Container : in out List);

   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element (
      Container : in out List;
      Position : Cursor;
      New_Item : Element_Type);

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (Element : Element_Type));

   --  modified
   procedure Update_Element (
      Container : in out List'Class; -- not primitive
      Position : Cursor;
      Process : not null access procedure (Element : in out Element_Type));

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is private
      with Implicit_Dereference => Element;

   type Reference_Type (
      Element : not null access Element_Type) is private
      with Implicit_Dereference => Element;

   function Constant_Reference (
      Container : aliased List;
      Position : Cursor)
      return Constant_Reference_Type;

   function Reference (
      Container : aliased in out List;
      Position : Cursor)
      return Reference_Type;

   procedure Assign (Target : in out List; Source : List);

   function Copy (Source : List) return List;

   procedure Move (Target : in out List; Source : in out List);

   procedure Insert (
      Container : in out List;
      Before : Cursor;
      New_Item : Element_Type;
      Count : Count_Type := 1);

   procedure Insert (
      Container : in out List;
      Before : Cursor;
      New_Item : Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1);

   procedure Insert (
      Container : in out List;
      Before : Cursor;
      Position : out Cursor;
      Count : Count_Type := 1);

   procedure Prepend (
      Container : in out List;
      New_Item : Element_Type;
      Count : Count_Type := 1);

   procedure Append (
      Container : in out List;
      New_Item : Element_Type;
      Count : Count_Type := 1);

   procedure Delete (
      Container : in out List;
      Position : in out Cursor;
      Count : Count_Type := 1);

   procedure Delete_First (Container : in out List; Count : Count_Type := 1);

   procedure Delete_Last (Container : in out List; Count : Count_Type := 1);

   procedure Reverse_Elements (Container : in out List);

   procedure Swap (Container : in out List; I, J : Cursor);

   procedure Swap_Links (Container : in out List; I, J : Cursor);

   procedure Splice (
      Target : in out List;
      Before : Cursor;
      Source : in out List);

   procedure Splice (
      Target : in out List;
      Before : Cursor;
      Source : in out List;
      Position : in out Cursor);

   procedure Splice (
      Container : in out List;
      Before : Cursor;
      Position : Cursor);

   function First (Container : List) return Cursor;

--  function First_Element (Container : List) return Element_Type;

   function Last (Container : List) return Cursor;

--  function Last_Element (Container : List) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   procedure Previous (Position : in out Cursor);

   --  modified
--  function Find (
--    Container : List;
--    Item : Element_Type;
--    Position : Cursor := No_Element)
--    return Cursor;
   function Find (
      Container : List;
      Item : Element_Type)
      return Cursor;
   function Find (
      Container : List;
      Item : Element_Type;
      Position : Cursor) return Cursor;

   --  modified
--  function Reverse_Find (
--    Container : List;
--    Item : Element_Type;
--    Position : Cursor := No_Element)
--    return Cursor;
   function Reverse_Find (
      Container : List;
      Item : Element_Type)
      return Cursor;
   function Reverse_Find (
      Container : List;
      Item : Element_Type;
      Position : Cursor)
      return Cursor;

   function Contains (Container : List; Item : Element_Type) return Boolean;

   --  extended
   function "<" (Left, Right : Cursor) return Boolean;

   --  modified
   procedure Iterate (
      Container : List'Class; -- not primitive
      Process : not null access procedure (Position : Cursor));

   --  modified
   procedure Reverse_Iterate (
      Container : List'Class; -- not primitive
      Process : not null access procedure (Position : Cursor));

   function Iterate (Container : List)
      return List_Iterator_Interfaces.Reversible_Iterator'Class;

--  function Iterate (Container : List; Start : Cursor)
--    return List_Iterator_Interfaces.Reversible_Iterator'Class;

   --  extended
   function Iterate (Container : List; First, Last : Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'Class;

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting is
      function Is_Sorted (Container : List) return Boolean;
      procedure Sort (Container : in out List);
      procedure Merge (Target : in out List; Source : in out List);
   end Generic_Sorting;

--  diff (Equivalents)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

private

   package Base renames Linked_Lists.Doubly;

--  diff (Element_Access)

   type Node is limited record
      Super : aliased Base.Node;
      Element : aliased Element_Type;
   end record;

   --  place Super at first whether Element_Type is controlled-type
   for Node use record
      Super at 0 range 0 .. Base.Node_Size - 1;
   end record;

   type Data is limited record
      Super : aliased Copy_On_Write.Data;
      First : Linked_Lists.Node_Access := null;
      Last : Linked_Lists.Node_Access := null;
      Length : Count_Type := 0;
   end record;

   type Data_Access is access Data;

   type List is new Finalization.Controlled with record
      Super : aliased Copy_On_Write.Container;
--  diff
--  diff
   end record;

   overriding procedure Adjust (Object : in out List);
   overriding procedure Finalize (Object : in out List)
      renames Clear;

   type Cursor is access Node;

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is null record;

   type Reference_Type (
      Element : not null access Element_Type) is null record;

   type List_Iterator is
      new List_Iterator_Interfaces.Reversible_Iterator with
   record
      First : Cursor;
      Last : Cursor;
   end record;

   overriding function First (Object : List_Iterator) return Cursor;
   overriding function Next (Object : List_Iterator; Position : Cursor)
      return Cursor;
   overriding function Last (Object : List_Iterator) return Cursor;
   overriding function Previous (Object : List_Iterator; Position : Cursor)
      return Cursor;

   package Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out List);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : List);

      procedure Missing_Read (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : out Cursor);
      procedure Missing_Write (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : Cursor);

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
         Item : out List_Iterator);
      function Missing_Input (
         Stream : not null access Streams.Root_Stream_Type'Class)
         return List_Iterator;
      procedure Missing_Write (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : List_Iterator);

      pragma Import (Ada, Missing_Read, "__drake_program_error");
      pragma Import (Ada, Missing_Input, "__drake_program_error");
      pragma Import (Ada, Missing_Write, "__drake_program_error");

   end Streaming;

   for List'Read use Streaming.Read;
   for List'Write use Streaming.Write;

   for Cursor'Read use Streaming.Missing_Read;
   for Cursor'Write use Streaming.Missing_Write;

   for Constant_Reference_Type'Read use Streaming.Missing_Read;
   for Constant_Reference_Type'Write use Streaming.Missing_Write;

   for Reference_Type'Read use Streaming.Missing_Read;
   for Reference_Type'Write use Streaming.Missing_Write;

   for List_Iterator'Read use Streaming.Missing_Read;
   for List_Iterator'Input use Streaming.Missing_Input;
   for List_Iterator'Write use Streaming.Missing_Write;
   for List_Iterator'Output use Streaming.Missing_Write;

   No_Element : constant Cursor := null;

end Ada.Containers.Doubly_Linked_Lists;
