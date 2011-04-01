pragma License (Unrestricted);
--  extended package
--  diff (Copy_On_Write)
private with Ada.Containers.Inside.Binary_Trees.Arne_Andersson;
private with Ada.Finalization;
--  diff (Streams)
generic
   type Key_Type (<>) is limited private;
   type Element_Type (<>) is limited private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
--  diff ("=")
package Ada.Containers.Limited_Ordered_Maps is
   pragma Preelaborate;
--  pragma Remote_Types; --  it defends to define Reference_Type...

   function Equivalent_Keys (Left, Right : Key_Type) return Boolean;

   type Map is tagged limited private;
   pragma Preelaborable_Initialization (Map);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

--  Empty_Map : constant Map;
   function Empty_Map return Map;

   No_Element : constant Cursor;

--  diff ("=")

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

--  diff (Element)

   function Floor (Container : Map; Key : Key_Type) return Cursor;

   function Ceiling (Container : Map; Key : Key_Type) return Cursor;

   function Contains (Container : Map; Key : Key_Type) return Boolean;

   function Has_Element (Position : Cursor) return Boolean;

   function "<" (Left, Right : Cursor) return Boolean;

--  function ">" (Left, Right : Cursor) return Boolean;

--  function "<" (Left : Cursor; Right : Key_Type) return Boolean;

--  function ">" (Left : Cursor; Right : Key_Type) return Boolean;

--  function "<" (Left : Key_Type; Right : Cursor) return Boolean;

--  function ">" (Left : Key_Type; Right : Cursor) return Boolean;

   procedure Iterate (
      Container : Map;
      Process : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate (
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
   function Constant_Reference (
      Container : not null access constant Map;
      Key : Key_Type)
      return Constant_Reference_Type;
   function Reference (
      Container : not null access Map;
      Position : Cursor)
      return Reference_Type;
   function Reference (
      Container : not null access Map;
      Key : Key_Type)
      return Reference_Type;

   --  AI05-0139-2
--  type Iterator_Type is new Reversible_Iterator with private;
   type Iterator is limited private;
   function First (Object : Iterator) return Cursor;
   function Next (Object : Iterator; Position : Cursor) return Cursor;
   function Last (Object : Iterator) return Cursor;
   function Previous (Object : Iterator; Position : Cursor) return Cursor;
   function Iterate (Container : not null access constant Map)
      return Iterator;

   --  extended
   generic
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
   package Equivalents is
      function "=" (Left, Right : Map) return Boolean;
   end Equivalents;

private

   package Binary_Trees renames Containers.Inside.Binary_Trees;
   package Base renames Binary_Trees.Arne_Andersson;
--  diff (Copy_On_Write)

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

--  diff (Data)
--
--
--
--

--  diff (Data_Access)

   type Map is new Finalization.Limited_Controlled with record
      Root : Binary_Trees.Node_Access := null;
      Length : Count_Type := 0;
   end record;

--  diff
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

end Ada.Containers.Limited_Ordered_Maps;
