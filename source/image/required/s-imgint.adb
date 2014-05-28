with System.Formatting;
package body System.Img_Int is
   pragma Suppress (All_Checks);

   procedure Image_Integer (
      V : Integer;
      S : in out String;
      P : out Natural)
   is
      X : Formatting.Unsigned;
      Error : Boolean;
   begin
      pragma Assert (S'Length >= 1);
      if V < 0 then
         S (S'First) := '-';
         X := Formatting.Unsigned'Mod (-V);
      else
         S (S'First) := ' ';
         X := Formatting.Unsigned (V);
      end if;
      Formatting.Image (
         X,
         S (S'First + 1 .. S'Last),
         P,
         Error => Error);
      pragma Assert (not Error);
   end Image_Integer;

end System.Img_Int;
