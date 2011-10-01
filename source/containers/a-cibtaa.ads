pragma License (Unrestricted);
--  implementation unit
package Ada.Containers.Inside.Binary_Trees.Arne_Andersson is
   pragma Preelaborate;

   Node_Size : constant := Standard'Address_Size * 3 + Integer'Size;

   type Node is limited record
      Super : aliased Binary_Trees.Node;
      Level : Integer;
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

end Ada.Containers.Inside.Binary_Trees.Arne_Andersson;
