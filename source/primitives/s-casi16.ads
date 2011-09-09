pragma License (Unrestricted);
--  implementation package required by compiler
package System.Compare_Array_Signed_16 is
   pragma Pure;

   --  required to compare arrays by compiler (s-casi16.ads)
   function Compare_Array_S16 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;

end System.Compare_Array_Signed_16;
