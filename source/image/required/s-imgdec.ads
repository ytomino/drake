pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Img_Dec is
   pragma Pure;

   --  required for Fixed'Image by compiler (s-imglld.ads)
   procedure Image_Decimal (
      V : Integer;
      S : in out String;
      P : out Natural;
      Scale : Integer);

end System.Img_Dec;
