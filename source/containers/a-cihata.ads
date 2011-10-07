pragma License (Unrestricted);
--  implementation unit
with System;
package Ada.Containers.Inside.Hash_Tables is
   pragma Preelaborate;

   Node_Size : constant := Standard'Address_Size + Hash_Type'Size * 2;

   type Node;
   type Node_Access is access Node;
   type Node is limited record
      Next : Node_Access;
      Hash : Hash_Type;
      Index : Hash_Type;
   end record;
   for Node'Size use Node_Size;

   type Entry_List is limited record
      First : Node_Access;
      Last : Node_Access;
   end record;

   type Entry_Array is array (Hash_Type range <>) of Entry_List;

   type Table (Last : Hash_Type) is limited record
      Entries : Entry_Array (0 .. Last);
   end record;

   type Table_Access is access Table;

   --  traversing

   function First (Container : Table_Access) return Node_Access;
   function Last (Container : Table_Access) return Node_Access;

   procedure Iterate (
      Container : Table_Access;
      Process : not null access procedure (Position : not null Node_Access));

   function Is_Before (Before, After : Node_Access) return Boolean;

   --  search

   function Find (
      Container : Table_Access;
      Hash : Hash_Type;
      Params : System.Address;
      Equivalent : not null access function (
         Position : not null Node_Access;
         Params : System.Address)
         return Boolean)
      return Node_Access;

   --  comparison

   function Equivalent (
      Left : Table_Access;
      Left_Length : Count_Type;
      Right : Table_Access;
      Right_Length : Count_Type;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Boolean;

   function Overlap (
      Left, Right : Table_Access;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Boolean;

   function Is_Subset (
      Subset, Of_Set : Table_Access;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Boolean;

   --  management

   function Capacity (Container : Table_Access) return Count_Type;

   procedure Free (
      Container : in out Table_Access;
      Length : in out Count_Type;
      Free : not null access procedure (Object : in out Node_Access));

   procedure Copy (
      Target : out Table_Access;
      Length : out Count_Type;
      Source : Table_Access;
      New_Capacity : Count_Type;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access));

   procedure Rebuild (
      Container : in out Table_Access;
      New_Capacity : Count_Type);

   procedure Insert (
      Container : in out Table_Access;
      Length : in out Count_Type;
      Hash : Hash_Type;
      New_Item : not null Node_Access);

   procedure Remove (
      Container : Table_Access;
      Length : in out Count_Type;
      Item : not null Node_Access);

   --  set operations

   procedure Merge (
      Target : in out Table_Access;
      Length : in out Count_Type;
      Source : Table_Access;
      Source_Length : Count_Type;
      In_Only_Left : Boolean;
      In_Only_Right : Boolean;
      In_Both : Boolean;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean;
      Copy : access procedure (
         Target : out Node_Access;
         Source : not null Node_Access);
      Free : access procedure (Object : in out Node_Access));

   procedure Merge (
      Target : out Table_Access;
      Length : out Count_Type;
      Left : Table_Access;
      Left_Length : Count_Type;
      Right : Table_Access;
      Right_Length : Count_Type;
      In_Only_Left : Boolean;
      In_Only_Right : Boolean;
      In_Both : Boolean;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access));

end Ada.Containers.Inside.Hash_Tables;
