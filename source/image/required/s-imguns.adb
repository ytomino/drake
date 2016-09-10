with System.Formatting;
package body System.Img_Uns is

   procedure Image_Unsigned (
      V : Unsigned_Types.Unsigned;
      S : in out String;
      P : out Natural)
   is
      Error : Boolean;
   begin
      pragma Assert (S'Length >= 1);
      S (S'First) := ' ';
      Formatting.Image (
         Formatting.Word_Unsigned (V),
         S (S'First + 1 .. S'Last),
         P,
         Error => Error);
      pragma Assert (not Error);
   end Image_Unsigned;

end System.Img_Uns;
