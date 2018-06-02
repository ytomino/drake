with System.Formatting;
with System.Img_Uns;
with System.Long_Long_Integer_Types;
package body System.Img_LLU is

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;
   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   procedure Image (
      V : Long_Long_Unsigned;
      S : in out String;
      P : out Natural);
   procedure Image (
      V : Long_Long_Unsigned;
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

   --  implementation

   procedure Image_Long_Long_Unsigned (
      V : Unsigned_Types.Long_Long_Unsigned;
      S : in out String;
      P : out Natural) is
   begin
      if Unsigned_Types.Long_Long_Unsigned'Size <= Standard'Word_Size then
         Img_Uns.Image (Word_Unsigned (V), S, P);
      else
         Image (Long_Long_Unsigned (V), S, P);
      end if;
   end Image_Long_Long_Unsigned;

end System.Img_LLU;
