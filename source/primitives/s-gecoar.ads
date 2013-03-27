pragma License (Unrestricted);
--  implementation unit
generic
   type Element_Type is (<>);
package System.Generic_Compare_Arrays is
   pragma Pure;

   function Compare (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;
   pragma Pure_Function (Compare);

end System.Generic_Compare_Arrays;
