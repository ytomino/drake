pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Long_Long_Integer_Types;
package System.Img_Int is
   pragma Pure;

   --  required for Integer'Image by compiler (s-imgint.ads)
   procedure Image_Integer (
      V : Integer;
      S : in out String;
      P : out Natural);

   --  helper
   procedure Image (
      V : Long_Long_Integer_Types.Word_Integer;
      S : in out String;
      P : out Natural);

end System.Img_Int;
