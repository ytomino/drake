pragma License (Unrestricted);
--  implementation package required by compiler
package System.Val_Char is
   pragma Pure;

   --  required for Character'Value by compiler (s-valcha.ads)
   function Value_Character (Str : String) return Character;

   --  helper
   HEX_Prefix : constant String := "HEX_"; --  upper case
   function Value_Named (S : String) return Character;

end System.Val_Char;
