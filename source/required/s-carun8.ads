pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Unsigned_8 is
   pragma Pure;

   type Unsigned_8 is mod 2 ** 8;
   for Unsigned_8'Size use 8;

   --  required to compare arrays by compiler (s-carun8.ads)
   function Compare_Array_U8_Unaligned is
      new Packed_Arrays.Compare (Unsigned_8);

   pragma Suppress (All_Checks); -- for instantiation

   --  required to compare arrays by compiler (s-carun8.ads)
   function Compare_Array_U8 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Compare_Array_U8_Unaligned;

end System.Compare_Array_Unsigned_8;
