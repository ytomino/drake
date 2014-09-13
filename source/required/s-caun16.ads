pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Unsigned_16 is
   pragma Pure;

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;
   for Unsigned_16'Alignment use 1;

   --  required to compare arrays by compiler (s-caun16.ads)
   function Compare_Array_U16 is
      new Packed_Arrays.Compare (Unsigned_16);

end System.Compare_Array_Unsigned_16;
