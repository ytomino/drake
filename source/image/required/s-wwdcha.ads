pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Wid_Char;
package System.WWd_Char is
   pragma Pure;

   --  required for Character'Wide_Width by compiler (s-wwwdcha.ads)
   function Wide_Width_Character (Lo, Hi : Character) return Natural
      renames Wid_Char.Width_Character;

   --  required for Character'Wide_Wide_Width by compiler (s-wwwdcha.ads)
   function Wide_Wide_Width_Character (Lo, Hi : Character) return Natural
      renames Wid_Char.Width_Character;

end System.WWd_Char;
