pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Signed_32 is
   pragma Pure;

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Integer_32'Size use 32;
   for Integer_32'Alignment use 1;

   --  required to compare arrays by compiler (s-casi32.ads)
   function Compare_Array_S32 is
      new Packed_Arrays.Compare (Integer_32);

end System.Compare_Array_Signed_32;
