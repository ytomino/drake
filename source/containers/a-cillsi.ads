pragma License (Unrestricted);
--  implementation package
package Ada.Containers.Inside.Linked_Lists.Singly is
   pragma Preelaborate;

   type Node is new Linked_Lists.Node;

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
