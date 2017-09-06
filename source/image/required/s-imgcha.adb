--  This implementation violates some ACATS intentionally.
--  Violated ACATS tests: C352001
with System.Formatting;
package body System.Img_Char is

   procedure Image_Character_05 (
      V : Character;
      S : in out String;
      P : out Natural) is
   begin
      case V is
         when Character'Val (0) .. Character'Val (16#1f#) =>
            declare
               Item : String_3
                  renames Image_00_1F (V);
               Item_Length : constant Natural := Length (Item);
            begin
               pragma Assert (S'Length >= Item_Length);
               P := S'First + Item_Length - 1;
               S (S'First .. P) := Item (1 .. Item_Length);
            end;
         when ' ' | '!' | '"' | '#' | '$' | '%' | '&' | '''
            | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/'
            | '0' .. '9' | ':' | ';' | '<' | '=' | '>' | '?'
            | '@' | 'A' .. 'Z' | '[' | '\' | ']' | '^' | '_'
            | '`' | 'a' .. 'z' | '{' | '|' | '}' | '~' =>
            pragma Assert (S'Length >= 3);
            P := S'First + 2;
            S (S'First) := ''';
            S (S'First + 1) := V;
            S (P) := ''';
         when Character'Val (16#7f#) =>
            pragma Assert (S'Length >= Image_7F'Length);
            P := S'First + Image_7F'Length - 1;
            S (S'First .. P) := Image_7F;
         when Character'Val (16#80#) .. Character'Val (16#ff#) =>
            pragma Assert (S'Length >= Hex_Prefix'Length);
            P := S'First + Hex_Prefix'Length - 1;
            S (S'First .. P) := Hex_Prefix;
            declare
               Error : Boolean;
            begin
               Formatting.Image (
                  Formatting.Word_Unsigned'(Character'Pos (V)),
                  S (P + 1 .. S'Last),
                  P,
                  Base => 16,
                  Width => 2,
                  Error => Error);
               pragma Assert (not Error);
            end;
      end case;
   end Image_Character_05;

   function Length (Item : String_3) return Natural is
   begin
      if Item (3) = Character'Val (0) then
         return 2;
      else
         return 3;
      end if;
   end Length;

end System.Img_Char;
