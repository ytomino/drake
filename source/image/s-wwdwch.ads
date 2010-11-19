pragma License (Unrestricted);
--  implementation package required by compiler
with System.Wid_WChar;
package System.Wwd_WChar is
   pragma Pure;

   --  required for Wide_Character'Wide_Width by compiler (s-wwwdwch.ads)
   function Wide_Width_Wide_Character (Lo, Hi : Wide_Character) return Natural
      renames Wid_WChar.Width_Wide_Character;

   --  required for Wide_Wide_Character'Wide_Width by compiler (s-wwwdwch.ads)
   function Wide_Width_Wide_Wide_Character (Lo, Hi : Wide_Wide_Character)
      return Natural
      renames Wid_WChar.Width_Wide_Wide_Character;

   --  required for Wide_Character'Wide_Wide_Width by compiler (s-wwwdwch.ads)
   function Wide_Wide_Width_Wide_Character (Lo, Hi : Wide_Character)
      return Natural
      renames Wid_WChar.Width_Wide_Character;

   --  required for Wide_Wide_Character'Wide_Wide_Width by compiler
   --  (s-wwwdwch.ads)
   function Wide_Wide_Width_Wide_Wide_Char (Lo, Hi : Wide_Wide_Character)
      return Natural
      renames Wid_WChar.Width_Wide_Wide_Character;

end System.Wwd_WChar;
