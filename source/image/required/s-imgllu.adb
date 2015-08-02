with System.Formatting;
package body System.Img_LLU is

   procedure Image_Long_Long_Unsigned (
      V : Unsigned_Types.Long_Long_Unsigned;
      S : in out String;
      P : out Natural)
   is
      Error : Boolean;
   begin
      pragma Assert (S'Length >= 1);
      S (S'First) := ' ';
      Formatting.Image (
         Formatting.Longest_Unsigned (V),
         S (S'First + 1 .. S'Last),
         P,
         Error => Error);
      pragma Assert (not Error);
   end Image_Long_Long_Unsigned;

end System.Img_LLU;
