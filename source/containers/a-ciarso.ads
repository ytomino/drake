pragma License (Unrestricted);
--  implementation package
package Ada.Containers.Inside.Array_Sorting is
   pragma Pure;

   function Is_Sorted (
      First, Last : Integer;
      LT : not null access function (Left, Right : Integer) return Boolean)
      return Boolean;

   procedure Insertion_Sort (
      First, Last : Integer;
      LT : not null access function (Left, Right : Integer) return Boolean;
      Swap : not null access procedure (I, J : Integer));

   procedure In_Place_Merge_Sort (
      First, Last : Integer;
      LT : not null access function (Left, Right : Integer) return Boolean;
      Swap : not null access procedure (I, J : Integer));

   --  merge [First .. Middle] and [Middle + 1 .. Last]
   procedure In_Place_Merge (
      First, Middle, Last : Integer;
      LT : not null access function (Left, Right : Integer) return Boolean;
      Swap : not null access procedure (I, J : Integer));

   procedure In_Place_Reverse (
      First, Last : Integer;
      Swap : not null access procedure (I, J : Integer));

end Ada.Containers.Inside.Array_Sorting;
