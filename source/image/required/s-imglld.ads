pragma License (Unrestricted);
--  implementation package required by compiler
package System.Img_LLD is
   pragma Pure;

   --  required for Fixed'Image by compiler (s-imglld.ads)
   procedure Image_Long_Long_Decimal (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural;
      Scale : Integer);

end System.Img_LLD;
