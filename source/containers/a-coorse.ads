pragma License (Unrestricted);
--  Ada 2005
private with Ada.Containers.Inside.Copy_On_Write;
private with Ada.Containers.Inside.Binary_Trees.Arne_Andersson;
private with Ada.Finalization;
private with Ada.Streams;
generic
   type Element_Type is private;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Ordered_Sets is
   pragma Preelaborate;
--  pragma Remote_Types; -- [gcc 4.5/4.6] it defends to define Reference_Type

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean;

   type Set is tagged private;
   pragma Preelaborable_Initialization (Set);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   --  modified
--  Empty_Set : constant Set;
   function Empty_Set return Set;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

--   package Set_Iterator_Interfaces is new
--     Ada.Iterator_Interfaces (Cursor, Has_Element);
   type Iterator is limited private;
   function First (Object : Iterator) return Cursor;
   function Next (Object : Iterator; Position : Cursor) return Cursor;
   function Last (Object : Iterator) return Cursor;
   function Previous (Object : Iterator; Position : Cursor) return Cursor;

   function "=" (Left, Right : Set) return Boolean;

   function Equivalent_Sets (Left, Right : Set) return Boolean;

   function To_Set (New_Item : Element_Type) return Set;

   function Length (Container : Set) return Count_Type;

   function Is_Empty (Container : Set) return Boolean;

   procedure Clear (Container : in out Set);

   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element (
      Container : in out Set;
      Position : Cursor;
      New_Item : Element_Type);

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (Element : Element_Type));

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is private;

   function Constant_Reference (
      Container : not null access constant Set; -- [gcc 4.5/4.6] aliased
      Position : Cursor)
      return Constant_Reference_Type;

   procedure Assign (Target : in out Set; Source : Set);

   function Copy (Source : Set) return Set;

   procedure Move (Target : in out Set; Source : in out Set);

   procedure Insert (
      Container : in out Set;
      New_Item : Element_Type;
      Position : out Cursor;
      Inserted : out Boolean);

   procedure Insert (
      Container : in out Set;
      New_Item : Element_Type);

   procedure Include (Container : in out Set; New_Item : Element_Type);

   procedure Replace (Container : in out Set; New_Item : Element_Type);

   procedure Exclude (Container : in out Set; Item : Element_Type);

   procedure Delete (Container : in out Set; Item : Element_Type);

   procedure Delete (Container : in out Set; Position : in out Cursor);

--  procedure Delete_First (Container : in out Set);

--  procedure Delete_Last (Container : in out Set);

   procedure Union (Target : in out Set; Source : Set);

   function Union (Left, Right : Set) return Set;

   function "or" (Left, Right : Set) return Set
      renames Union;

   procedure Intersection (Target : in out Set; Source : Set);

   function Intersection (Left, Right : Set) return Set;

   function "and" (Left, Right : Set) return Set
      renames Intersection;

   procedure Difference (Target : in out Set; Source : Set);

   function Difference (Left, Right : Set) return Set;

   function "-" (Left, Right : Set) return Set
      renames Difference;

   procedure Symmetric_Difference (Target : in out Set; Source : Set);

   function Symmetric_Difference (Left, Right : Set) return Set;

   function "xor" (Left, Right : Set) return Set
      renames Symmetric_Difference;

   function Overlap (Left, Right : Set) return Boolean;

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean;

   function First (Container : Set) return Cursor;

--  function First_Element (Container : Set) return Element_Type;

   function Last (Container : Set) return Cursor;

--  function Last_Element (Container : Set) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   function Find (Container : Set; Item : Element_Type) return Cursor;

   function Floor (Container : Set; Item : Element_Type) return Cursor;

   function Ceiling (Container : Set; Item : Element_Type) return Cursor;

   function Contains (Container : Set; Item : Element_Type) return Boolean;

   function "<" (Left, Right : Cursor) return Boolean;

--  function ">" (Left, Right : Cursor) return Boolean;

   function "<" (Left : Cursor; Right : Element_Type) return Boolean;

--  function ">" (Left : Cursor; Right : Element_Type) return Boolean;

--  function "<" (Left : Element_Type; Right : Cursor) return Boolean;

--  function ">" (Left : Element_Type; Right : Cursor) return Boolean;

   --  modified
   procedure Iterate (
      Container : Set'Class; -- not primitive
      Process : not null access procedure (Position : Cursor));

   --  modified
   procedure Reverse_Iterate (
      Container : Set'Class; -- not primitive
      Process : not null access procedure (Position : Cursor));

--  function Iterate (Container : Set)
--    return Map_Iterator_Interfaces.Reversible_Iterator'Class;
   function Iterate (Container : Set)
      return Iterator;

   generic
      type Key_Type (<>) is private;
      with function Key (Element : Element_Type) return Key_Type;
      with function "<" (Left, Right : Key_Type) return Boolean is <>;
   package Generic_Keys is

      function Equivalent_Keys (Left, Right : Key_Type) return Boolean;

      function Key (Position : Cursor) return Key_Type;

      function Element (Container : Set; Key : Key_Type) return Element_Type;

      procedure Replace (
         Container : in out Set;
         Key : Key_Type;
         New_Item : Element_Type);

      procedure Exclude (Container : in out Set; Key : Key_Type);

      procedure Delete (Container : in out Set; Key : Key_Type);

      function Find (Container : Set; Key : Key_Type) return Cursor;

      function Floor (Container : Set; Key : Key_Type) return Cursor;

      function Ceiling (Container : Set; Key : Key_Type) return Cursor;

      function Contains (Container : Set; Key : Key_Type) return Boolean;

      procedure Update_Element_Preserving_Key (
         Container : in out Set;
         Position : Cursor;
         Process : not null access procedure (Element : in out Element_Type));

      type Reference_Type (
         Element : not null access Element_Type) is private;

      function Reference_Preserving_Key (
         Container : not null access Set; -- [gcc 4.5/4.6] aliased
         Position : Cursor)
         return Reference_Type;

      function Constant_Reference (
         Container : not null access constant Set; -- [gcc 4.5/4.6] aliased
         Key : Key_Type)
         return Constant_Reference_Type;

      function Reference_Preserving_Key (
         Container : not null access Set; -- [gcc 4.5/4.6] aliased
         Key : Key_Type)
         return Reference_Type;

   private

      type Reference_Type (
         Element : not null access Element_Type) is null record;

   end Generic_Keys;

   --  extended
   generic
      type Index_Type is (<>);
      type Element_Array is array (Index_Type range <>) of Element_Type;
   function Generic_Array_To_Set (S : Element_Array) return Set;

private

   package Binary_Trees renames Containers.Inside.Binary_Trees;
   package Base renames Binary_Trees.Arne_Andersson;
   package Copy_On_Write renames Containers.Inside.Copy_On_Write;

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
      Root : Binary_Trees.Node_Access := null;
      Length : Count_Type := 0;
   end record;

   type Data_Access is access Data;

   type Set is new Finalization.Controlled with record
      Super : aliased Copy_On_Write.Container;
--  diff
   end record;

   overriding procedure Adjust (Object : in out Set);
   overriding procedure Finalize (Object : in out Set)
      renames Clear;

   package No_Primitives is
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : out Set);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : Set);
   end No_Primitives;

   for Set'Read use No_Primitives.Read;
   for Set'Write use No_Primitives.Write;

   type Cursor is access Node;

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is null record;

   type Iterator is not null access constant Set;

   No_Element : constant Cursor := null;

end Ada.Containers.Ordered_Sets;
