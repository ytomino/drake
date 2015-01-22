with System.Formatting;
with System.Img_Char;
with System.Val_Enum;
with System.Value_Error;
package body System.Val_Char is
   pragma Suppress (All_Checks);
   use type Formatting.Unsigned;

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
                  if not Error
                     and then Used_Last = Last
                     and then Result <= Character'Pos (Character'Last)
                  then
                     return Character'Val (Result);
                  end if;
               end;
            else
               declare
                  Result : Character;
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
      Value_Error ("Character", Str);
   end Value_Character;

   procedure Get_Named (
      S : String;
      Value : out Character;
      Error : out Boolean) is
   begin
      for I in Img_Char.Image_00_1F'Range loop
         declare
            Item : Img_Char.String_3
               renames Img_Char.Image_00_1F (I);
            Item_Length : constant Natural := Img_Char.Length (Item);
         begin
            if S = Item (1 .. Item_Length) then
               Value := I;
               Error := False;
               return;
            end if;
         end;
      end loop;
      if S = Img_Char.Image_7F then
         Value := Character'Val (16#7f#);
         Error := False;
      else
         Error := True;
      end if;
   end Get_Named;

end System.Val_Char;
