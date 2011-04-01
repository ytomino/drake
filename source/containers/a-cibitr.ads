pragma License (Unrestricted);
--  implementation package
with System;
package Ada.Containers.Inside.Binary_Trees is
   pragma Preelaborate;

   type Node;
   type Node_Access is access Node;
   type Node is limited record
      Left, Right, Parent : Node_Access;
   end record;

   --  traversing

   function First (Container : Node_Access) return Node_Access;
   function Next (Item : not null Node_Access) return Node_Access;

   function Last (Container : Node_Access) return Node_Access;
   function Previous (Item : not null Node_Access) return Node_Access;

   procedure Iterate (
      Container : Node_Access;
      Process : not null access procedure (Position : not null Node_Access));

   procedure Iterate (
      Container : Node_Access;
      Params : System.Address;
      Process : not null access procedure (
         Position : not null Node_Access;
         Params : System.Address));

   procedure Reverse_Iterate (
      Container : Node_Access;
      Process : not null access procedure (Position : not null Node_Access));

   --  binary search

   type Find_Mode is (Just, Floor, Ceiling);
   pragma Discard_Names (Find_Mode);

   function Find (
      Container : Node_Access;
      Mode : Find_Mode;
      Params : System.Address;
      Compare : not null access function (
         Right : not null Node_Access;
         Params : System.Address)
         return Integer)
      return Node_Access;

   --  comparison

   function Equivalent (
      Left, Right : Node_Access;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Boolean;

   function Overlap (
      Left, Right : Node_Access;
      Compare : not null access function (Left, Right : not null Node_Access)
         return Integer)
      return Boolean;

   function Is_Subset (
      Subset, Of_Set : Node_Access;
      Compare : not null access function (Left, Right : not null Node_Access)
         return Integer)
      return Boolean;

   --  management

   procedure Free (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Free : not null access procedure (Object : in out Node_Access));

   --  set operations

   procedure Merge (
      Target : in out Node_Access;
      Length : in out Count_Type;
      Source : Node_Access;
      In_Only_Left : Boolean;
      In_Only_Right : Boolean;
      In_Both : Boolean;
      Compare : not null access function (Left, Right : not null Node_Access)
         return Integer;
      Copy : access procedure (
         Target : out Node_Access;
         Source : not null Node_Access);
      Insert : access procedure (
         Container : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access);
      Remove : access procedure (
         Container : in out Node_Access;
         Length : in out Count_Type;
         Position : not null Node_Access);
      Free : access procedure (Object : in out Node_Access));

   procedure Merge (
      Target : out Node_Access;
      Length : out Count_Type;
      Left : Node_Access;
      Right : Node_Access;
      In_Only_Left : Boolean;
      In_Only_Right : Boolean;
      In_Both : Boolean;
      Compare : not null access function (Left, Right : not null Node_Access)
         return Integer;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access);
      Insert : not null access procedure (
         Container : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access));

end Ada.Containers.Inside.Binary_Trees;
