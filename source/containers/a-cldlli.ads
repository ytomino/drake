pragma License (Unrestricted);
--  extended package
--  diff (Copy_On_Write)
private with Ada.Containers.Inside.Linked_Lists.Doubly;
private with Ada.Finalization;
--  diff (Streams)
generic
   type Element_Type (<>) is limited private;
--  diff ("=")
package Ada.Containers.Limited_Doubly_Linked_Lists is
   pragma Preelaborate;
--  pragma Remote_Types; --  it defends to define Reference_Type...

   type List is tagged limited private;
   pragma Preelaborable_Initialization (List);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

--  Empty_List : constant List;
   function Empty_List return List; --  extended

--  No_Element : constant Cursor;
   function No_Element return Cursor; --  extended

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

   procedure Update_Element (
      Container : in out List;
      Position : Cursor;
      Process : not null access procedure (Element : in out Element_Type));

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
      New_Item : not null access function (C : List) return Element_Type;
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

--  diff (Contains)

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

   --  extended
   generic
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
   package Equivalents is
      function "=" (Left, Right : List) return Boolean;
      function Find (Container : List; Item : Element_Type) return Cursor;
      function Find (Container : List;
                     Item : Element_Type;
                     Position : Cursor) return Cursor;
      function Reverse_Find (Container : List;
                             Item : Element_Type) return Cursor;
      function Reverse_Find (Container : List;
                             Item : Element_Type;
                             Position : Cursor) return Cursor;
      function Contains (Container : List; Item : Element_Type) return Boolean;
   end Equivalents;

private

   package Linked_Lists renames Containers.Inside.Linked_Lists;
   package Base renames Linked_Lists.Doubly;
--  diff (Copy_On_Write)

   type Element_Access is access Element_Type;

   type Node is limited record
      Super : aliased Base.Node;
      Element : Element_Access;
   end record;

   --  place Super at first whether Element_Type is controlled-type
   for Node use record
      Super at 0 range 0 .. Base.Node_Size - 1;
   end record;

   type Cursor is access Node;

--  diff (Data)
--
--
--
--
--

--  diff (Data_Access)

   type List is new Finalization.Limited_Controlled with record
      First : Linked_Lists.Node_Access := null;
      Last : Linked_Lists.Node_Access := null;
      Length : Count_Type := 0;
   end record;

--  diff (Adjust)
   overriding procedure Finalize (Object : in out List)
      renames Clear;

--  different line (stream attributes are unimplemented)
--
--
--
--
--
--
--

--  diff ('Read)
--  diff ('Write)

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is limited null record;

   type Reference_Type (
      Element : not null access Element_Type) is limited null record;

   type Iterator is not null access constant List;

end Ada.Containers.Limited_Doubly_Linked_Lists;
