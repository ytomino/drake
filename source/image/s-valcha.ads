pragma License (Unrestricted);
--  implementation package required by compiler
package System.Val_Char is
   pragma Pure;

   --  required for Character'Value by compiler (s-valcha.ads)
   function Value_Character (Str : String) return Character;

end System.Val_Char;
