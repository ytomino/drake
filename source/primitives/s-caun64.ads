pragma License (Unrestricted);
--  implementation package required by compiler
package System.Compare_Array_Unsigned_64 is
   pragma Pure;

   --  required to compare arrays by compiler (s-caun64.ads)
   function Compare_Array_U64 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;

end System.Compare_Array_Unsigned_64;
