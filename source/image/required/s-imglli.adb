with System.Formatting;
with System.Unsigned_Types;
package body System.Img_LLI is
   pragma Suppress (All_Checks);

   procedure Image_Long_Long_Integer (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural)
   is
      X : Unsigned_Types.Long_Long_Unsigned;
      Error : Boolean;
   begin
      pragma Assert (S'Length >= 1);
      if V < 0 then
         S (S'First) := '-';
         X := Unsigned_Types.Long_Long_Unsigned'Mod (-V);
      else
         S (S'First) := ' ';
         X := Unsigned_Types.Long_Long_Unsigned (V);
      end if;
      Formatting.Image (
         Formatting.Longest_Unsigned (X),
         S (S'First + 1 .. S'Last),
         P,
         Error => Error);
      pragma Assert (not Error);
   end Image_Long_Long_Integer;

end System.Img_LLI;
