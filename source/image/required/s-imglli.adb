with System.Formatting;
with System.Img_Int;
with System.Long_Long_Integer_Types;
package body System.Img_LLI is
   use type Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Word_Integer is Long_Long_Integer_Types.Word_Integer;
   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   procedure Image (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural);
   procedure Image (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural)
   is
      X : Long_Long_Unsigned;
      Error : Boolean;
   begin
      pragma Assert (S'Length >= 1);
      if V < 0 then
         S (S'First) := '-';
         X := -Long_Long_Unsigned'Mod (V);
      else
         S (S'First) := ' ';
         X := Long_Long_Unsigned (V);
      end if;
      Formatting.Image (X, S (S'First + 1 .. S'Last), P, Error => Error);
      pragma Assert (not Error);
   end Image;

   --  implementation

   procedure Image_Long_Long_Integer (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural) is
   begin
      if Long_Long_Integer'Size <= Standard'Word_Size then
         Img_Int.Image (Word_Integer (V), S, P);
      else
         Image (V, S, P);
      end if;
   end Image_Long_Long_Integer;

end System.Img_LLI;
