pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Img_Bool is
   pragma Pure;

   --  required for Boolean'Image by compiler (s-imgboo.ads)
   procedure Image_Boolean (
      V : Boolean;
      S : in out String;
      P : out Natural);

end System.Img_Bool;
