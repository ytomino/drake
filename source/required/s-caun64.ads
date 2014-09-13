pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Unsigned_64 is
   pragma Pure;

   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;
   for Unsigned_64'Alignment use 1;

   --  required to compare arrays by compiler (s-caun64.ads)
   function Compare_Array_U64 is
      new Packed_Arrays.Compare (Unsigned_64);

end System.Compare_Array_Unsigned_64;
