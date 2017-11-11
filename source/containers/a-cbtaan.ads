pragma License (Unrestricted);
--  implementation unit
package Ada.Containers.Binary_Trees.Arne_Andersson is
   pragma Preelaborate;

   subtype Level_Type is Count_Type'Base;

   Node_Size : constant :=
      Standard'Address_Size * 3
      + (Level_Type'Size + Standard'Address_Size - 1)
         / Standard'Address_Size
         * Standard'Address_Size;

   type Node is limited record
      Super : aliased Binary_Trees.Node;
      Level : Level_Type;
   end record;

   for Node'Size use Node_Size;

   procedure Insert (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Before : Node_Access;
      New_Item : not null Node_Access);

   procedure Remove (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Position : not null Node_Access);

   procedure Copy (
      Target : out Node_Access;
      Length : out Count_Type;
      Source : Node_Access;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access));

   --  set operations

   procedure Merge is
      new Binary_Trees.Merge (Insert => Insert, Remove => Remove);

   procedure Copying_Merge is
      new Binary_Trees.Copying_Merge (Insert => Insert);

end Ada.Containers.Binary_Trees.Arne_Andersson;
