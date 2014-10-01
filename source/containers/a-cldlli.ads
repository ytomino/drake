pragma License (Unrestricted);
--  extended unit
with Ada.Iterator_Interfaces;
--  diff (Copy_On_Write)
private with Ada.Containers.Linked_Lists;
private with Ada.Containers.Linked_Lists.Doubly;
private with Ada.Finalization;
private with Ada.Streams;
generic
   type Element_Type (<>) is limited private;
--  diff ("=")
package Ada.Containers.Limited_Doubly_Linked_Lists is
   pragma Preelaborate;
   pragma Remote_Types;

   type List is tagged limited private
      with
         Constant_Indexing => Constant_Reference,
         Variable_Indexing => Reference,
         Default_Iterator => Iterate,
         Iterator_Element => Element_Type;
   pragma Preelaborable_Initialization (List);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

--  diff
--  Empty_List : constant List;
   function Empty_List return List;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   package List_Iterator_Interfaces is
      new Iterator_Interfaces (Cursor, Has_Element);

--  diff ("=")

   function Length (Container : List) return Count_Type;

   function Is_Empty (Container : List) return Boolean;

   procedure Clear (Container : in out List);

--  diff (Element)

--  diff (Replace_Element)
--
--
--

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

--  diff (Assign)

--  diff (Copy)

   procedure Move (Target : in out List; Source : in out List);

--  diff (Insert)
--
--
--
--

   procedure Insert (
      Container : in out List;
      Before : Cursor;
      New_Item : not null access function return Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1);

--  diff (Insert)
--
--
--
--

--  diff (Prepend)
--
--
--

--  diff (Append)
--
--
--

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

--  diff (First_Element)

   function Last (Container : List) return Cursor;

--  diff (Last_Element)

   function Next (Position : Cursor) return Cursor;

   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   procedure Previous (Position : in out Cursor);

--  diff (Find)
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Reverse_Find)
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Contains)

   --  extended
   function "<" (Left, Right : Cursor) return Boolean;

   --  modified
   procedure Iterate (
      Container : List'Class; -- not primitive
      Process : not null access procedure (Position : Cursor));

   --  modified
   procedure Reverse_Iterate (
      Container : List; -- not primitive
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

   generic
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
   package Equivalents is
      function "=" (Left, Right : List) return Boolean;
      function Find (Container : List; Item : Element_Type) return Cursor;
      function Find (Container : List; Item : Element_Type; Position : Cursor)
         return Cursor;
      function Reverse_Find (Container : List; Item : Element_Type)
         return Cursor;
      function Reverse_Find (
         Container : List;
         Item : Element_Type;
         Position : Cursor)
         return Cursor;
      function Contains (Container : List; Item : Element_Type) return Boolean;
   end Equivalents;

private

   package Base renames Linked_Lists.Doubly;

   type Element_Access is access Element_Type;

   type Node is limited record
      Super : aliased Base.Node;
      Element : Element_Access;
   end record;

   --  place Super at first whether Element_Type is controlled-type
   for Node use record
      Super at 0 range 0 .. Base.Node_Size - 1;
   end record;

--  diff (Data)
--
--
--
--
--

--  diff (Data_Access)

   type List is limited new Finalization.Limited_Controlled with record
      First : Linked_Lists.Node_Access := null;
      Last : Linked_Lists.Node_Access := null;
      Length : Count_Type := 0;
   end record;

--  diff (Adjust)
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

--  diff (Read)
--
--
--  diff (Write)
--
--

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

--  diff ('Read)
--  diff ('Write)

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

end Ada.Containers.Limited_Doubly_Linked_Lists;
