--  This implementation violates some ACATS intentionally.
--  Violated ACATS tests: C352001
with System.Formatting;
package body System.Img_Char is
   pragma Suppress (All_Checks);

   procedure Image_Character (
      V : Character;
      S : in out String;
      P : out Natural) is
   begin
      case V is
         when Character'Val (0) .. Character'Val (16#1f#) =>
            pragma Assert (S'Length >= Images_1f (V).all'Length);
            P := S'First - 1 + Images_1f (V).all'Length;
            S (S'First .. P) := Images_1f (V).all;
         when ' ' | '!' | '"' | '#' | '$' | '%' | '&' | '''
            | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/'
            | '0' .. '9' | ':' | ';' | '<' | '=' | '>' | '?'
            | '@' | 'A' .. 'Z' | '[' | '\' | ']' | '^' | '_'
            | '`' | 'a' .. 'z' | '{' | '|' | '}' | '~'
         =>
            pragma Assert (S'Length >= 3);
            P := S'First + 2;
            S (S'First) := ''';
            S (S'First + 1) := V;
            S (P) := ''';
         when Character'Val (16#7f#) =>
            pragma Assert (S'Length >= Image_7f'Length);
            P := S'First - 1 + Image_7f'Length;
            S (S'First .. P) := Image_7f;
         when Character'Val (16#80#) .. Character'Val (16#ff#) =>
            pragma Assert (S'Length >= Hex_Prefix'Length);
            S (S'First .. S'First - 1 + Hex_Prefix'Length) := Hex_Prefix;
            declare
               Error : Boolean;
            begin
               Formatting.Image (
                  Formatting.Unsigned'(Character'Pos (V)),
                  S (S'First + Hex_Prefix'Length .. S'Last),
                  P,
                  Base => 16,
                  Width => 2,
                  Error => Error);
               pragma Assert (not Error);
            end;
      end case;
   end Image_Character;

end System.Img_Char;
