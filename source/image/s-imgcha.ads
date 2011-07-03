pragma License (Unrestricted);
--  implementation package required by compiler
package System.Img_Char is
   pragma Pure;

   --  required for Character'Image by compiler (s-imgcha.ads)
   procedure Image_Character_05 (
      V : Character;
      S : in out String;
      P : out Natural);

   --  helper
   Hex_Prefix : constant String := "Hex_";
   Images_1f : constant array (Character'Val (0) .. Character'Val (16#1f#)) of
      not null access constant String := (
      new String'("NUL"),
      new String'("SOH"),
      new String'("STX"),
      new String'("ETX"),
      new String'("EOT"),
      new String'("ENQ"),
      new String'("ACK"),
      new String'("BEL"),
      new String'("BS"),
      new String'("HT"),
      new String'("LF"),
      new String'("VT"),
      new String'("FF"),
      new String'("CR"),
      new String'("SO"),
      new String'("SI"),
      new String'("DLE"),
      new String'("DC1"),
      new String'("DC2"),
      new String'("DC3"),
      new String'("DC4"),
      new String'("NAK"),
      new String'("SYN"),
      new String'("ETB"),
      new String'("CAN"),
      new String'("EM"),
      new String'("SUB"),
      new String'("ESC"),
      new String'("FS"),
      new String'("GS"),
      new String'("RS"),
      new String'("US"));
   Image_7f : constant String := "DEL";

end System.Img_Char;
