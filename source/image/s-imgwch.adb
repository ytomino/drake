with System.Formatting;
with System.Img_Char;
package body System.Img_WChar is
   pragma Suppress (All_Checks);

   procedure Image_Wide_Character (
      V : Wide_Character;
      S : in out String;
      P : out Natural;
      Ada_2005 : Boolean)
   is
      pragma Unreferenced (Ada_2005);
   begin
      case V is
         when Wide_Character'Val (0) .. Wide_Character'Val (16#7f#) =>
            Img_Char.Image_Character (
               Character'Val (Wide_Character'Pos (V)),
               S,
               P);
         when Wide_Character'Val (16#ad#) =>
            pragma Assert (S'Length >= Image_ad'Length);
            P := S'First - 1 + Image_ad'Length;
            S (S'First .. P) := Image_ad;
         when others =>
            pragma Assert (S'Length >= Img_Char.Hex_Prefix'Length);
            S (S'First .. S'First - 1 + Img_Char.Hex_Prefix'Length) :=
               Img_Char.Hex_Prefix;
            declare
               Error : Boolean;
            begin
               Formatting.Image (
                  Formatting.Unsigned'(Wide_Character'Pos (V)),
                  S (S'First + Img_Char.Hex_Prefix'Length .. S'Last),
                  P,
                  Base => 16,
                  Width => 4,
                  Error => Error);
               pragma Assert (not Error);
            end;
      end case;
   end Image_Wide_Character;

   procedure Image_Wide_Wide_Character (
      V : Wide_Wide_Character;
      S : in out String;
      P : out Natural) is
   begin
      case V is
         when Wide_Wide_Character'Val (0) ..
            Wide_Wide_Character'Val (16#7f#)
         =>
            Img_Char.Image_Character (
               Character'Val (Wide_Wide_Character'Pos (V)),
               S,
               P);
         when Wide_Wide_Character'Val (16#ad#) =>
            Image_Wide_Character (
               Wide_Character'Val (Wide_Wide_Character'Pos (V)),
               S,
               P,
               Ada_2005 => True);
         when others =>
            pragma Assert (S'Length >= Img_Char.Hex_Prefix'Length);
            S (S'First .. S'First - 1 + Img_Char.Hex_Prefix'Length) :=
               Img_Char.Hex_Prefix;
            declare
               Error : Boolean;
            begin
               Formatting.Image (
                  Formatting.Unsigned'(Wide_Wide_Character'Pos (V)),
                  S (S'First + Img_Char.Hex_Prefix'Length .. S'Last),
                  P,
                  Base => 16,
                  Width => 8,
                  Error => Error);
               pragma Assert (not Error);
            end;
      end case;
   end Image_Wide_Wide_Character;

end System.Img_WChar;
