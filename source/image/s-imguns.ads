pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
package System.Img_Uns is
   pragma Pure;

   --  required for Modular'Image by compiler (s-imguns.ads)
   procedure Image_Unsigned (
      V : Unsigned_Types.Unsigned;
      S : in out String;
      P : out Natural);

end System.Img_Uns;
