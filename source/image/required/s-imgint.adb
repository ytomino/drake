with System.Formatting;
package body System.Img_Int is
   use type Long_Long_Integer_Types.Word_Integer;
   use type Long_Long_Integer_Types.Word_Unsigned;

   subtype Word_Integer is Long_Long_Integer_Types.Word_Integer;
   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   --  implementation

   procedure Image_Integer (
      V : Integer;
      S : in out String;
      P : out Natural) is
   begin
      Image (Word_Integer (V), S, P);
   end Image_Integer;

   procedure Image (
      V : Long_Long_Integer_Types.Word_Integer;
      S : in out String;
      P : out Natural)
   is
      X : Word_Unsigned;
      Error : Boolean;
   begin
      pragma Assert (S'Length >= 1);
      if V < 0 then
         S (S'First) := '-';
         X := -Word_Unsigned'Mod (V);
      else
         S (S'First) := ' ';
         X := Word_Unsigned (V);
      end if;
      Formatting.Image (X, S (S'First + 1 .. S'Last), P, Error => Error);
      pragma Assert (not Error);
   end Image;

end System.Img_Int;
