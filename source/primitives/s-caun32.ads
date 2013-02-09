pragma License (Unrestricted);
--  implementation package required by compiler
package System.Compare_Array_Unsigned_32 is
   pragma Pure;

   --  required to compare arrays by compiler (s-caun32.ads)
   function Compare_Array_U32 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;
   pragma Pure_Function (Compare_Array_U32);

end System.Compare_Array_Unsigned_32;
