pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Signed_16 is
   pragma Pure;

   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;
   for Integer_16'Alignment use 1;

   --  required to compare arrays by compiler (s-casi16.ads)
   function Compare_Array_S16 is
      new Packed_Arrays.Compare (Integer_16);

end System.Compare_Array_Signed_16;
