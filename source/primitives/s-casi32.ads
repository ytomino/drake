pragma License (Unrestricted);
--  implementation package required by compiler
package System.Compare_Array_Signed_32 is
   pragma Pure;

   --  required to compare arrays by compiler (s-casi32.ads)
   function Compare_Array_S32 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;
   pragma Pure_Function (Compare_Array_S32);

end System.Compare_Array_Signed_32;
