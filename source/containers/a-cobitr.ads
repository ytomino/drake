pragma License (Unrestricted);
--  implementation unit
with System;
private package Ada.Containers.Binary_Trees is
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

   --  traversing in leaf-to-root order

   function Leaf_To_Root_First (Container : Node_Access) return Node_Access;
   function Leaf_To_Root_Next (Item : not null Node_Access)
      return Node_Access;

   --  traversing in root-to-leaf order

   type Index_Type is (Left, Right);
   pragma Discard_Names (Index_Type);

   procedure Root_To_Leaf_Next (
      Previous_Source_Item : not null Node_Access;
      Source_Parent_Item : out Node_Access;
      Previous_Target_Item : not null Node_Access;
      Target_Parent_Item : out Node_Access;
      Index : out Index_Type);

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
         Left, Right : not null Node_Access)
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

   type Containing is (In_Only_Left, In_Only_Right, In_Both);
   pragma Discard_Names (Containing);

   type Filter_Type is array (Containing) of Boolean;
   pragma Pack (Filter_Type);
   pragma Suppress_Initialization (Filter_Type);

   procedure Merge (
      Target : in out Node_Access;
      Length : in out Count_Type;
      Source : Node_Access;
      Filter : Filter_Type;
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
      Left, Right : Node_Access;
      Filter : Filter_Type;
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

end Ada.Containers.Binary_Trees;
