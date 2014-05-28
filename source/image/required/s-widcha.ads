pragma License (Unrestricted);
--  implementation package required by compiler
package System.Wid_Char is
   pragma Pure;

   --  required for Character'Width by compiler (s-widboo.ads)
   function Width_Character (Lo, Hi : Character) return Natural;

   --  compiler return 12 ("Reserved_153") for statically character types

end System.Wid_Char;
