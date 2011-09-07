pragma License (Unrestricted);
--  Ada 2005
private with Ada.Containers.Inside.Copy_On_Write;
private with Ada.Containers.Inside.Linked_Lists.Doubly;
private with Ada.Finalization;
private with Ada.Streams;
generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Doubly_Linked_Lists is
   pragma Preelaborate;
--  pragma Remote_Types; -- it defends to define Reference_Type...

   type List is tagged private;
   pragma Preelaborable_Initialization (List);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   --  modified
--  Empty_List : constant List;
   function Empty_List return List;

   No_Element : constant Cursor;

   function "=" (Left, Right : List) return Boolean;

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

   procedure Update_Element (
      Container : in out List;
      Position : Cursor;
      Process : not null access procedure (Element : in out Element_Type));

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

--  diff (Insert)
--
--
--
--

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

   function First_Element (Container : List) return Element_Type;

   function Last (Container : List) return Cursor;

   function Last_Element (Container : List) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   procedure Previous (Position : in out Cursor);

--  function Find (
--    Container : List;
--    Item : Element_Type;
--    Position : Cursor := No_Element)
--    return Cursor;
   --  alternative
   function Find (Container : List; Item : Element_Type) return Cursor;
   function Find (
      Container : List;
      Item : Element_Type;
      Position : Cursor) return Cursor;

--  function Reverse_Find (
--    Container : List;
--    Item : Element_Type;
--    Position : Cursor := No_Element)
--    return Cursor;
   --  alternative
   function Reverse_Find (
      Container : List;
      Item : Element_Type) return Cursor;
   function Reverse_Find (
      Container : List;
      Item : Element_Type;
      Position : Cursor) return Cursor;

   function Contains (Container : List; Item : Element_Type) return Boolean;

   function Has_Element (Position : Cursor) return Boolean;

   procedure Iterate (
      Container : List;
      Process : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate (
      Container : List;
      Process : not null access procedure (Position : Cursor));

   --  AI05-0212-1
   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is limited private;
   type Reference_Type (
      Element : not null access Element_Type) is limited private;
   function Constant_Reference (
      Container : not null access constant List;
      Position : Cursor)
      return Constant_Reference_Type;
   function Reference (
      Container : not null access List;
      Position : Cursor)
      return Reference_Type;

   --  AI05-0139-2
--  type Iterator_Type is new Reversible_Iterator with private;
   type Iterator is limited private;
   function First (Object : Iterator) return Cursor;
   function Next (Object : Iterator; Position : Cursor) return Cursor;
   function Last (Object : Iterator) return Cursor;
   function Previous (Object : Iterator; Position : Cursor) return Cursor;
   function Iterate (Container : not null access constant List)
      return Iterator;

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

private

   package Linked_Lists renames Containers.Inside.Linked_Lists;
   package Base renames Linked_Lists.Doubly;
   package Copy_On_Write renames Containers.Inside.Copy_On_Write;

   type Element_Access is access Element_Type;

   type Node is limited record
      Super : aliased Base.Node;
      Element : Element_Access;
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

   package No_Primitives is
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : out List);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : List);
   end No_Primitives;

   for List'Read use No_Primitives.Read;
   for List'Write use No_Primitives.Write;

   type Cursor is access Node;

   No_Element : constant Cursor := null;

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is null record;

   type Reference_Type (
      Element : not null access Element_Type) is null record;

   type Iterator is not null access constant List;

end Ada.Containers.Indefinite_Doubly_Linked_Lists;
