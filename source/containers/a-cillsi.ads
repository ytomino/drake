pragma License (Unrestricted);
--  implementation unit
package Ada.Containers.Inside.Linked_Lists.Singly is
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

end Ada.Containers.Inside.Linked_Lists.Singly;
