pragma License (Unrestricted);
--  implementation package required by compiler
package System.Compare_Array_Unsigned_16 is
   pragma Pure;

   --  required to compare arrays by compiler (s-caun16.ads)
   function Compare_Array_U16 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;

end System.Compare_Array_Unsigned_16;
