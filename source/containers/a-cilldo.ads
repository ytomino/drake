pragma License (Unrestricted);
--  implementation unit
package Ada.Containers.Inside.Linked_Lists.Doubly is
   pragma Preelaborate;

   Node_Size : constant := Standard'Address_Size * 2;

   type Node is limited record
      Super : aliased Linked_Lists.Node;
      Next : Node_Access;
   end record;

   for Node'Size use Node_Size;

   procedure Iterate (
      First : Node_Access;
      Process : not null access procedure (Position : not null Node_Access));

   function Find (
      First : Node_Access;
      Params : System.Address;
      Equivalent : not null access function (
         Right : not null Node_Access;
         Params : System.Address)
         return Boolean)
      return Node_Access;

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

   procedure Swap_Links (
      First : in out Node_Access;
      Last : in out Node_Access;
      I, J : not null Node_Access);

   procedure Splice (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      Before : Node_Access;
      Source_First : in out Node_Access;
      Source_Last : in out Node_Access;
      Source_Length : in out Count_Type);

   procedure Split (
      Target_First : out Node_Access;
      Target_Last : out Node_Access;
      Length : out Count_Type;
      Source_First : in out Node_Access;
      Source_Last : in out Node_Access;
      Source_Length : in out Count_Type;
      Count : Count_Type);

end Ada.Containers.Inside.Linked_Lists.Doubly;
