pragma License (Unrestricted);
--  implementation unit
with System.Long_Long_Integer_Types;
private package Ada.Containers.Array_Sorting is
   pragma Pure;

   function Is_Sorted (
      First, Last : System.Long_Long_Integer_Types.Word_Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address)
         return Boolean)
      return Boolean;

   procedure Insertion_Sort (
      First, Last : System.Long_Long_Integer_Types.Word_Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address)
         return Boolean;
      Swap : not null access procedure (
         I, J : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address));

   procedure In_Place_Merge_Sort (
      First, Last : System.Long_Long_Integer_Types.Word_Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address)
         return Boolean;
      Swap : not null access procedure (
         I, J : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address));

   --  merge [First .. Before - 1] and [Before .. Last]
   procedure In_Place_Merge (
      First, Before, Last : System.Long_Long_Integer_Types.Word_Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address)
         return Boolean;
      Swap : not null access procedure (
         I, J : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address));

   --  following subprograms are in order to implement sorting

   --  reverse all elements
   procedure In_Place_Reverse (
      First, Last : System.Long_Long_Integer_Types.Word_Integer;
      Params : System.Address;
      Swap : not null access procedure (
         I, J : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address));

   --  swap [First .. Before - 1] and [Before .. Last] with double reversing
   procedure Reverse_Rotate (
      First, Before, Last : System.Long_Long_Integer_Types.Word_Integer;
      Params : System.Address;
      Swap : not null access procedure (
         I, J : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address));

   --  swap [First .. Before - 1] and [Before .. Last] with juggling
   procedure Juggling_Rotate (
      First, Before, Last : System.Long_Long_Integer_Types.Word_Integer;
      Params : System.Address;
      Swap : not null access procedure (
         I, J : System.Long_Long_Integer_Types.Word_Integer;
         Params : System.Address));

end Ada.Containers.Array_Sorting;
