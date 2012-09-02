pragma License (Unrestricted);
--  implementation unit
with System;
package Ada.Containers.Inside.Array_Sorting is
   pragma Pure;

   function Is_Sorted (
      First, Last : Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : Integer;
         Params : System.Address)
         return Boolean)
      return Boolean;

   procedure Insertion_Sort (
      First, Last : Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : Integer;
         Params : System.Address)
         return Boolean;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address));

   procedure In_Place_Merge_Sort (
      First, Last : Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : Integer;
         Params : System.Address)
         return Boolean;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address));

   --  merge [First .. Middle] and [Middle + 1 .. Last]
   procedure In_Place_Merge (
      First, Middle, Last : Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : Integer;
         Params : System.Address)
         return Boolean;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address));

   --  following subprograms are in order to implement sorting

   --  reverse all elements
   procedure In_Place_Reverse (
      First, Last : Integer;
      Params : System.Address;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address));

   --  swap [First .. Middle] and [Middle + 1 .. Last] with double reversing
   procedure Reverse_Rotate (
      First, Middle, Last : Integer;
      Params : System.Address;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address));

   --  swap [First .. Middle] and [Middle + 1 .. Last] with juggling
   procedure Juggling_Rotate (
      First, Middle, Last : Integer;
      Params : System.Address;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address));

end Ada.Containers.Inside.Array_Sorting;
