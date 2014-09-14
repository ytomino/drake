pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Signed_64 is
   pragma Pure;

   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for Integer_64'Size use 64;
   for Integer_64'Alignment use 1;

   --  required to compare arrays by compiler (s-casi64.ads)
   function Compare_Array_S64 is
      new Packed_Arrays.Compare (Integer_64);

end System.Compare_Array_Signed_64;
