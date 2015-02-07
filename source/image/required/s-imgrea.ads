pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Img_Real is
   pragma Pure;

   --  required for Fixed'Image by compiler (s-imgrea.ads)
   procedure Image_Ordinary_Fixed_Point (
      V : Long_Long_Float;
      S : in out String;
      P : out Natural;
      Aft : Natural);

   --  required for Float'Image by compiler (s-imgrea.ads)
   procedure Image_Floating_Point (
      V : Long_Long_Float;
      S : in out String;
      P : out Natural;
      Digs : Natural); -- T'Digits

end System.Img_Real;
