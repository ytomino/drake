with System.Formatting;
with System.Img_WChar;
with System.Val_Char;
with System.Val_Enum;
with System.UTF_Conversions;
package body System.Val_WChar is
   pragma Suppress (All_Checks);
   use type Formatting.Unsigned;

   function Value_Named (S : String) return Wide_Character is
   begin
      if S = Img_WChar.Image_ad then
         return Wide_Character'Val (16#ad#);
      end if;
      return Wide_Character'Val (Character'Pos (Val_Char.Value_Named (S)));
   end Value_Named;

   function Value_Wide_Character (Str : String; EM : WC_Encoding_Method)
      return Wide_Character
   is
      pragma Unreferenced (EM);
      First : Positive;
      Last : Natural;
   begin
      Val_Enum.Trim (Str, First, Last);
      if First + 2 <= Last
         and then Str (First) = '''
         and then Str (Last) = '''
      then
         declare
            Used_Last : Natural;
            Code : UTF_Conversions.UCS_4;
            Error : Boolean;
            Result : Wide_String (1 .. 2);
         begin
            UTF_Conversions.From_UTF_8 (
               Str (First + 1 .. Last),
               Used_Last,
               Code,
               Error);
            if Error or else Used_Last + 1 /= Last then
               raise Constraint_Error;
            end if;
            UTF_Conversions.To_UTF_16 (
               Code,
               Result,
               Used_Last,
               Error);
            if Error or else Used_Last /= 1 then
               raise Constraint_Error;
            end if;
            return Result (1);
         end;
      else
         declare
            S : String := Str (First .. Last);
         begin
            Val_Enum.To_Upper (S);
            if S'Length >= Val_Char.HEX_Prefix'Length
               and then S (
                  First ..
                  First - 1 + Val_Char.HEX_Prefix'Length) =
                  Val_Char.HEX_Prefix
            then
               declare
                  Used_Last : Natural;
                  Result : Formatting.Unsigned;
                  Error : Boolean;
               begin
                  Formatting.Value (
                     S (First + Val_Char.HEX_Prefix'Length .. Last),
                     Used_Last,
                     Result,
                     Base => 16,
                     Error => Error);
                  if Error
                     or else Used_Last /= Last
                     or else Result > Wide_Character'Pos (Wide_Character'Last)
                  then
                     raise Constraint_Error;
                  end if;
                  return Wide_Character'Val (Result);
               end;
            else
               return Value_Named (S);
            end if;
         end;
      end if;
   end Value_Wide_Character;

   function Value_Wide_Wide_Character (Str : String; EM : WC_Encoding_Method)
      return Wide_Wide_Character
   is
      pragma Unreferenced (EM);
      First : Positive;
      Last : Natural;
   begin
      Val_Enum.Trim (Str, First, Last);
      if First + 2 <= Last
         and then Str (First) = '''
         and then Str (Last) = '''
      then
         declare
            Used_Last : Natural;
            Code : UTF_Conversions.UCS_4;
            Error : Boolean;
         begin
            UTF_Conversions.From_UTF_8 (
               Str (First + 1 .. Last),
               Used_Last,
               Code,
               Error);
            if Error or else Used_Last + 1 /= Last then
               raise Constraint_Error;
            end if;
            return Wide_Wide_Character'Val (Code);
         end;
      else
         declare
            S : String := Str (First .. Last);
         begin
            Val_Enum.To_Upper (S);
            if S'Length >= Val_Char.HEX_Prefix'Length
               and then S (
                  First ..
                  First - 1 + Val_Char.HEX_Prefix'Length) =
                  Val_Char.HEX_Prefix
            then
               declare
                  Used_Last : Natural;
                  Result : Formatting.Unsigned;
                  Error : Boolean;
               begin
                  Formatting.Value (
                     S (First + Val_Char.HEX_Prefix'Length .. Last),
                     Used_Last,
                     Result,
                     Base => 16,
                     Error => Error);
                  if Error
                     or else Used_Last /= Last
                     or else Result >
                        Wide_Wide_Character'Pos (Wide_Wide_Character'Last)
                  then
                     raise Constraint_Error;
                  end if;
                  return Wide_Wide_Character'Val (Result);
               end;
            else
               return Wide_Wide_Character'Val (Wide_Character'Pos (
                  Value_Named (S)));
            end if;
         end;
      end if;
   end Value_Wide_Wide_Character;

end System.Val_WChar;
