pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Val_Char is
   pragma Pure;

   --  required for Character'Value by compiler (s-valcha.ads)
   function Value_Character (Str : String) return Character;

   --  helper
   HEX_Prefix : constant String := "HEX_"; -- upper case
   procedure Get_Named (
      S : String;
      Value : out Character;
      Error : out Boolean);

end System.Val_Char;
