pragma License (Unrestricted);
--  extended unit
--  diff (Copy_On_Write)
private with Ada.Containers.Inside.Hash_Tables;
private with Ada.Finalization;
--  diff (Streams)
generic
   type Element_Type (<>) is limited private;
   with function Hash (Element : Element_Type) return Hash_Type;
   with function Equivalent_Elements (Left, Right : Element_Type)
      return Boolean;
--  diff ("=")
package Ada.Containers.Limited_Hashed_Sets is
   pragma Preelaborate;
--  pragma Remote_Types; -- it defends to define Reference_Type...

   type Set is tagged limited private;
   pragma Preelaborable_Initialization (Set);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

--  diff
--  Empty_Set : constant Set;
   function Empty_Set return Set;

   No_Element : constant Cursor;

--  diff ("=")

   function Equivalent_Sets (Left, Right : Set) return Boolean;

--  diff (To_Set)

   function Capacity (Container : Set) return Count_Type;

   procedure Reserve_Capacity (
      Container : in out Set;
      Capacity : Count_Type);

   function Length (Container : Set) return Count_Type;

   function Is_Empty (Container : Set) return Boolean;

   procedure Clear (Container : in out Set);

--  diff (Element)

--  diff (Replace_Element)
--
--
--

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (Element : Element_Type));

--  diff (Assign)

--  diff (Copy)

   procedure Move (Target : in out Set; Source : in out Set);

   procedure Insert (
      Container : in out Set;
      New_Item : not null access function (C : Set) return Element_Type;
      Position : out Cursor);
--  diff

--  diff (Insert)

--  diff (Include)

--  diff (Replace)

   procedure Exclude (Container : in out Set; Item : Element_Type);

   procedure Delete (Container : in out Set; Item : Element_Type);

   procedure Delete (Container : in out Set; Position : in out Cursor);

--  diff (Union)

--  diff (Union)

--  diff ("or")
--

   procedure Intersection (Target : in out Set; Source : Set);

--  diff (Intersection)

--  diff ("and")
--

   procedure Difference (Target : in out Set; Source : Set);

--  diff (Difference)

--  diff ("-")
--

--  diff (Symmetric_Difference)

--  diff (Symmetric_Difference)

--  diff ("xor")
--

   function Overlap (Left, Right : Set) return Boolean;

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean;

   function First (Container : Set) return Cursor;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Find (Container : Set; Item : Element_Type) return Cursor;

   function Contains (Container : Set; Item : Element_Type) return Boolean;

   function Has_Element (Position : Cursor) return Boolean;

--  function Equivalent_Elements (Left, Right : Cursor) return Boolean;

   function Equivalent_Elements (Left : Cursor; Right : Element_Type)
      return Boolean;

--  function Equivalent_Elements (Left : Element_Type; Right : Cursor)
--    return Boolean;

   procedure Iterate (
      Container : Set;
      Process : not null access procedure (Position : Cursor));

   --  AI05-0212-1
   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is limited private;
   type Reference_Type (
      Element : not null access Element_Type) is limited private;
   function Constant_Reference (
      Container : not null access constant Set;
      Position : Cursor)
      return Constant_Reference_Type;
   function Reference (
      Container : not null access Set;
      Position : Cursor)
      return Reference_Type;

   --  AI05-0139-2
--  type Iterator_Type is new Forward_Iterator with private;
   type Iterator is limited private;
   function First (Object : Iterator) return Cursor;
   function Next (Object : Iterator; Position : Cursor) return Cursor;
   function Iterate (Container : not null access constant Set)
      return Iterator;

   generic
      type Key_Type (<>) is private;
      with function Key (Element : Element_Type) return Key_Type;
      with function Hash (Key : Key_Type) return Hash_Type;
      with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   package Generic_Keys is

      function Key (Position : Cursor) return Key_Type;

--  diff (Element)

--  diff (Replace)
--
--
--

      procedure Exclude (Container : in out Set; Key : Key_Type);

      procedure Delete (Container : in out Set; Key : Key_Type);

      function Find (Container : Set; Key : Key_Type) return Cursor;

      function Contains (Container : Set; Key : Key_Type) return Boolean;

      procedure Update_Element_Preserving_Key (
         Container : in out Set;
         Position : Cursor;
         Process : not null access procedure (Element : in out Element_Type));

   end Generic_Keys;

   --  extended
   generic
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
   package Equivalents is
      function "=" (Left, Right : Set) return Boolean;
   end Equivalents;

private

   package Hash_Tables renames Containers.Inside.Hash_Tables;
--  diff

   type Element_Access is access Element_Type;

   type Node is limited record
      Super : aliased Hash_Tables.Node;
      Element : Element_Access;
   end record;

   --  place Super at first whether Element_Type is controlled-type
   for Node use record
      Super at 0 range 0 .. Hash_Tables.Node_Size - 1;
   end record;

--  diff (Data)
--
--
--
--

--  diff (Data_Access)

   type Set is new Finalization.Limited_Controlled with record
      Table : Hash_Tables.Table_Access;
      Length : Count_Type := 0;
   end record;

--  diff (Adjust)
   overriding procedure Finalize (Object : in out Set)
      renames Clear;

--  diff (No_Primitives)
--
--
--
--
--
--
--

--  diff ('Read)
--  diff ('Write)

   type Cursor is access Node;

   No_Element : constant Cursor := null;

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is null record;

   type Reference_Type (
      Element : not null access Element_Type) is null record;

   type Iterator is not null access constant Set;

end Ada.Containers.Limited_Hashed_Sets;
