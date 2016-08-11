pragma License (Unrestricted);
--  implementation unit
with System;
private package Ada.Containers.Linked_Lists is
   pragma Preelaborate;

   type Node;
   type Node_Access is access Node;
   type Node is limited record
      Previous : Node_Access;
   end record;

   --  traversing

   procedure Reverse_Iterate (
      Last : Node_Access;
      Process : not null access procedure (Position : not null Node_Access));

   --  liner search

   function Reverse_Find (
      Last : Node_Access;
      Params : System.Address;
      Equivalent : not null access function (
         Right : not null Node_Access;
         Params : System.Address)
         return Boolean)
      return Node_Access;

   --  comparison

   function Equivalent (
      Left_Last, Right_Last : Node_Access;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean;

   --  management

   procedure Free (
      First : in out Node_Access;
      Last : in out Node_Access;
      Length : in out Count_Type;
      Free : not null access procedure (Object : in out Node_Access));

   generic
      with procedure Insert (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access);
   procedure Copy (
      Target_First : out Node_Access;
      Target_Last : out Node_Access;
      Length : out Count_Type;
      Source_Last : Node_Access;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access));

   generic
      with procedure Insert (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access);
      with procedure Remove (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Position : not null Node_Access;
         Next : Node_Access);
   procedure Reverse_Elements (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type);

   --  sorting

   function Is_Sorted (
      Last : Node_Access;
      LT : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean;

   generic
      with procedure Insert (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access);
      with procedure Remove (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Position : not null Node_Access;
         Next : Node_Access);
   procedure Merge (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      Source_First : in out Node_Access;
      Source_Last : in out Node_Access;
      Source_Length : in out Count_Type;
      LT : not null access function (
         Left, Right : not null Node_Access)
         return Boolean);

   generic
      with procedure Split (
         Target_First : out Node_Access;
         Target_Last : out Node_Access;
         Length : out Count_Type;
         Source_First : in out Node_Access;
         Source_Last : in out Node_Access;
         Source_Length : in out Count_Type;
         Count : Count_Type);
      with procedure Merge (
         Target_First : in out Node_Access;
         Target_Last : in out Node_Access;
         Length : in out Count_Type;
         Source_First : in out Node_Access;
         Source_Last : in out Node_Access;
         Source_Length : in out Count_Type;
         LT : not null access function (
            Left, Right : not null Node_Access)
            return Boolean);
   procedure Merge_Sort (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      LT : not null access function (
         Left, Right : not null Node_Access)
         return Boolean);

end Ada.Containers.Linked_Lists;
