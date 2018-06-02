pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Long_Long_Integer_Types;
with System.Unsigned_Types;
package System.Img_Uns is
   pragma Pure;

   --  required for Modular'Image by compiler (s-imguns.ads)
   procedure Image_Unsigned (
      V : Unsigned_Types.Unsigned;
      S : in out String;
      P : out Natural);

   --  helper
   procedure Image (
      V : Long_Long_Integer_Types.Word_Unsigned;
      S : in out String;
      P : out Natural);

end System.Img_Uns;
