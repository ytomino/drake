pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Unsigned_32 is
   pragma Pure;

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;
   for Unsigned_32'Alignment use 1;

   --  required to compare arrays by compiler (s-caun32.ads)
   function Compare_Array_U32 is
      new Packed_Arrays.Compare (Unsigned_32);

end System.Compare_Array_Unsigned_32;
