pragma License (Unrestricted);
--  extended unit
--  diff (Copy_On_Write)
private with Ada.Containers.Inside.Hash_Tables;
private with Ada.Finalization;
--  diff (Streams)
generic
   type Key_Type (<>) is limited private;
   type Element_Type (<>) is limited private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
--  diff ("=")
package Ada.Containers.Limited_Hashed_Maps is
   pragma Preelaborate;
--  pragma Remote_Types; -- it defends to define Reference_Type...

   type Map is tagged limited private;
   pragma Preelaborable_Initialization (Map);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

--  diff
--  Empty_Map : constant Map;
   function Empty_Map return Map;

   No_Element : constant Cursor;

--  diff ("=")

   function Capacity (Container : Map) return Count_Type;

   procedure Reserve_Capacity (Container : in out Map; Capacity : Count_Type);

   function Length (Container : Map) return Count_Type;

   function Is_Empty (Container : Map) return Boolean;

   procedure Clear (Container : in out Map);

--  diff (Key)

--  diff (Element)

--  diff (Replace_Element)
--
--
--

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : Element_Type));

   procedure Update_Element (
      Container : in out Map;
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : in out Element_Type));

--  diff (Assign)

--  diff (Copy)

   procedure Move (Target : in out Map; Source : in out Map);

   procedure Insert (
      Container : in out Map;
      New_Key : not null access function (C : Map) return Key_Type;
      New_Item : not null access function (C : Map) return Element_Type;
      Position : out Cursor);
--  diff

--  diff (Insert)
--
--
--
--

--  diff (Insert)
--
--
--

--  diff (Include)
--
--
--

--  diff (Replace)
--
--
--

   procedure Exclude (Container : in out Map; Key : Key_Type);

   procedure Delete (Container : in out Map; Key : Key_Type);

   procedure Delete (Container : in out Map; Position : in out Cursor);

   function First (Container : Map) return Cursor;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Find (Container : Map; Key : Key_Type) return Cursor;

--  diff (Element)

   function Contains (Container : Map; Key : Key_Type) return Boolean;

   function Has_Element (Position : Cursor) return Boolean;

--  function Equivalent_Keys (Left, Right : Cursor) return Boolean;

   function Equivalent_Keys (Left : Cursor; Right : Key_Type) return Boolean;

--  function Equivalent_Keys (Left : Key_Type; Right : Cursor) return Boolean;

   procedure Iterate (
      Container : Map;
      Process : not null access procedure (Position : Cursor));

   --  AI05-0212-1
   type Constant_Reference_Type (
      Key : not null access constant Key_Type;
      Element : not null access constant Element_Type) is limited private;
   type Reference_Type (
      Key : not null access constant Key_Type;
      Element : not null access Element_Type) is limited private;
   function Constant_Reference (
      Container : not null access constant Map;
      Position : Cursor)
      return Constant_Reference_Type;
   function Reference (
      Container : not null access Map;
      Position : Cursor)
      return Reference_Type;

   --  AI05-0139-2
--  type Iterator_Type is new Forward_Iterator with private;
   type Iterator is limited private;
   function First (Object : Iterator) return Cursor;
   function Next (Object : Iterator; Position : Cursor) return Cursor;
   function Iterate (Container : not null access constant Map)
      return Iterator;

   generic
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
   package Equivalent is
      function "=" (Left, Right : Map) return Boolean;
   end Equivalent;

private

   package Hash_Tables renames Containers.Inside.Hash_Tables;
--  diff (Copy_On_Write)

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

--  diff (Data)
--
--
--
--

--  diff (Data_Access)

   type Map is new Finalization.Limited_Controlled with record
      Table : Hash_Tables.Table_Access;
      Length : Count_Type := 0;
   end record;

--  diff (Adjust)
   overriding procedure Finalize (Object : in out Map)
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
      Key : not null access constant Key_Type;
      Element : not null access constant Element_Type) is null record;

   type Reference_Type (
      Key : not null access constant Key_Type;
      Element : not null access Element_Type) is null record;

   type Iterator is not null access constant Map;

end Ada.Containers.Limited_Hashed_Maps;
