pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Img_Char is
   pragma Pure;

   --  required for Character'Image by compiler (s-imgcha.ads)
   procedure Image_Character_05 (
      V : Character;
      S : in out String;
      P : out Natural);

   --  helper
   Hex_Prefix : constant String := "Hex_";
   subtype String_3 is String (1 .. 3);
   function Length (Item : String_3) return Natural;
   pragma Inline (Length);
   Image_00_1F : constant array (
      Character'Val (0) ..
      Character'Val (16#1F#)) of String_3 := (
         "NUL",
         "SOH",
         "STX",
         "ETX",
         "EOT",
         "ENQ",
         "ACK",
         "BEL",
         "BS" & Character'Val (0),
         "HT" & Character'Val (0),
         "LF" & Character'Val (0),
         "VT" & Character'Val (0),
         "FF" & Character'Val (0),
         "CR" & Character'Val (0),
         "SO" & Character'Val (0),
         "SI" & Character'Val (0),
         "DLE",
         "DC1",
         "DC2",
         "DC3",
         "DC4",
         "NAK",
         "SYN",
         "ETB",
         "CAN",
         "EM" & Character'Val (0),
         "SUB",
         "ESC",
         "FS" & Character'Val (0),
         "GS" & Character'Val (0),
         "RS" & Character'Val (0),
         "US" & Character'Val (0));
   Image_7F : constant String_3 := "DEL";

end System.Img_Char;
