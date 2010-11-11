pragma License (Unrestricted);
--  extended package
package Ada.UCD is
   pragma Pure;

   type UCS_4 is mod 16#80000000#; --  same as System.UTF_Conversions.UCS_4

   type UCS_4_Array is array (Positive range <>) of UCS_4;
   for UCS_4_Array'Component_Size use 32;

   subtype UCS_2 is UCS_4 range 0 .. 16#ffff#;

   type UCS_2_Array is array (Positive range <>) of UCS_2;
   for UCS_2_Array'Component_Size use 16;

end Ada.UCD;
