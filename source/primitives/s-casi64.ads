pragma License (Unrestricted);
--  implementation package required by compiler
package System.Compare_Array_Signed_64 is
   pragma Pure;

   --  required to compare arrays by compiler (s-casi64.ads)
   function Compare_Array_S64 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;
   pragma Pure_Function (Compare_Array_S64);

end System.Compare_Array_Signed_64;
