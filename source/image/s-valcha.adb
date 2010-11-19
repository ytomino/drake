with System.Formatting;
with System.Img_Char;
with System.Val_Enum;
pragma Warnings (Off);
with System.Debug;
package body System.Val_Char is
   pragma Suppress (All_Checks);

   function Value_Character (Str : String) return Character is
      First : Positive;
      Last : Natural;
   begin
      Val_Enum.Trim (Str, First, Last);
      if First + 2 = Last
         and then Str (First) = '''
         and then Str (Last) = '''
      then
         return Str (First + 1);
      else
         declare
            S : String := Str (First .. Last);
         begin
            Val_Enum.To_Upper (S);
            if S'Length >= HEX_Prefix'Length
               and then S (First .. First - 1 + HEX_Prefix'Length) =
                  HEX_Prefix
            then
               declare
                  Used_Last : Natural;
                  Result : Formatting.Unsigned;
                  Error : Boolean;
               begin
                  Formatting.Value (
                     S (First + HEX_Prefix'Length .. Last),
                     Used_Last,
                     Result,
                     Base => 16,
                     Error => Error);
                  if Error or else Used_Last /= Last then
                     raise Constraint_Error;
                  end if;
                  return Character'Val (Result);
               end;
            else
               return Value_Named (S);
            end if;
         end;
      end if;
   end Value_Character;

   function Value_Named (S : String) return Character is
   begin
      for I in Img_Char.Images_1f'Range loop
         if S = Img_Char.Images_1f (I).all then
            return I;
         end if;
      end loop;
      if S = Img_Char.Image_7f then
         return Character'Val (16#7f#);
      end if;
      raise Constraint_Error;
   end Value_Named;

end System.Val_Char;
