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
      if V < Wide_Character'Val (16#7f#) then
         Img_Char.Image_Character (
            Character'Val (Wide_Character'Pos (V)),
            S,
            P);
      else
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
               Padding => '0',
               Error => Error);
            pragma Assert (not Error);
         end;
      end if;
   end Image_Wide_Character;

   procedure Image_Wide_Wide_Character (
      V : Wide_Wide_Character;
      S : in out String;
      P : out Natural) is
   begin
      if V < Wide_Wide_Character'Val (16#7f#) then
         Img_Char.Image_Character (
            Character'Val (Wide_Wide_Character'Pos (V)),
            S,
            P);
      else
         pragma Assert (S'Length >= Img_Char.Hex_Prefix'Length);
         S (S'First .. S'First - 1 + Img_Char.Hex_Prefix'Length) :=
            Img_Char.Hex_Prefix;
         declare
            Error : Boolean;
         begin
            Formatting.Image (
               Formatting.Unsigned'(Wide_Wide_Character'Pos (V)),
               S,
               P,
               Base => 16,
               Width => 8,
               Padding => '0',
               Error => Error);
            pragma Assert (not Error);
         end;
      end if;
   end Image_Wide_Wide_Character;

end System.Img_WChar;
