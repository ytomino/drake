pragma License (Unrestricted);
--  implementation unit
package Ada.Containers.Linked_Lists.Singly is
   pragma Preelaborate;

   Node_Size : constant := Standard'Address_Size;

   type Node is new Linked_Lists.Node;

   for Node'Size use Node_Size;

   function Is_Before (Before, After : Node_Access) return Boolean;

   procedure Insert (
      First : in out Node_Access;
      Last : in out Node_Access;
      Length : in out Count_Type;
      Before : Node_Access;
      New_Item : not null Node_Access);

   procedure Remove (
      First : in out Node_Access;
      Last : in out Node_Access;
      Length : in out Count_Type;
      Position : not null Node_Access;
      Next : Node_Access);

   procedure Split (
      Target_First : out Node_Access;
      Target_Last : out Node_Access;
      Length : out Count_Type;
      Source_First : in out Node_Access;
      Source_Last : in out Node_Access;
      Source_Length : in out Count_Type;
      Count : Count_Type);

   procedure Copy is new Linked_Lists.Copy (Insert => Insert);

   procedure Reverse_Elements is
      new Linked_Lists.Reverse_Elements (Insert => Insert, Remove => Remove);

   --  sorting

   procedure Merge is
      new Linked_Lists.Merge (Insert => Insert, Remove => Remove);

   procedure Merge_Sort is
      new Linked_Lists.Merge_Sort (Split => Split, Merge => Merge);

end Ada.Containers.Linked_Lists.Singly;
