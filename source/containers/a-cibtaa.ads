pragma License (Unrestricted);
--  implementation package
package Ada.Containers.Inside.Binary_Trees.Arne_Andersson is
   pragma Preelaborate;

   type Node is limited record
      Super : aliased Binary_Trees.Node;
      Level : Integer;
   end record;

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
