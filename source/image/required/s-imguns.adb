with System.Formatting;
package body System.Img_Uns is

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   --  implementation

   procedure Image_Unsigned (
      V : Unsigned_Types.Unsigned;
      S : in out String;
      P : out Natural) is
   begin
      Image (Word_Unsigned (V), S, P);
   end Image_Unsigned;

   procedure Image (
      V : Long_Long_Integer_Types.Word_Unsigned;
      S : in out String;
      P : out Natural)
   is
      Error : Boolean;
   begin
      pragma Assert (S'Length >= 1);
      S (S'First) := ' ';
      Formatting.Image (V, S (S'First + 1 .. S'Last), P, Error => Error);
      pragma Assert (not Error);
   end Image;

end System.Img_Uns;
