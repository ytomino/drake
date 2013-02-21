with System.Formatting;
with System.Img_WChar;
with System.Val_Char;
with System.Val_Enum;
with System.Value_Error;
with System.UTF_Conversions;
package body System.Val_WChar is
   pragma Suppress (All_Checks);
   use type Formatting.Unsigned;

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
            if not Error and then Used_Last + 1 = Last then
               UTF_Conversions.To_UTF_16 (
                  Code,
                  Result,
                  Used_Last,
                  Error);
               if not Error and then Used_Last = 1 then
                  return Result (1);
               end if;
            end if;
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
                  if not Error
                     and then Used_Last = Last
                     and then Result <=
                        Wide_Character'Pos (Wide_Character'Last)
                  then
                     return Wide_Character'Val (Result);
                  end if;
               end;
            else
               declare
                  Result : Wide_Character;
                  Error : Boolean;
               begin
                  Get_Named (S, Result, Error);
                  if not Error then
                     return Result;
                  end if;
               end;
            end if;
         end;
      end if;
      Value_Error ("Wide_Character", Str);
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
            if not Error and then Used_Last + 1 = Last then
               return Wide_Wide_Character'Val (Code);
            end if;
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
                  if not Error
                     and then Used_Last = Last
                     and then Result <=
                        Wide_Wide_Character'Pos (Wide_Wide_Character'Last)
                  then
                     return Wide_Wide_Character'Val (Result);
                  end if;
               end;
            else
               declare
                  Result : Wide_Character;
                  Error : Boolean;
               begin
                  Get_Named (S, Result, Error);
                  if not Error then
                     return Wide_Wide_Character'Val (Wide_Character'Pos (
                        Result));
                  end if;
               end;
            end if;
         end;
      end if;
      Value_Error ("Wide_Wide_Character", Str);
   end Value_Wide_Wide_Character;

   procedure Get_Named (
      S : String;
      Value : out Wide_Character;
      Error : out Boolean) is
   begin
      if S = Img_WChar.Image_ad then
         Value := Wide_Character'Val (16#ad#);
         Error := False;
      else
         declare
            C : Character;
         begin
            Val_Char.Get_Named (S, C, Error);
            Value := Wide_Character'Val (Character'Pos (C));
         end;
      end if;
   end Get_Named;

end System.Val_WChar;
