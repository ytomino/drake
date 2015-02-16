pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Img_Enum_New is
   pragma Pure;

   --  required for Enum'Image by compiler (s-imenne.ads)
   procedure Image_Enumeration_8 (
      Pos : Natural;
      S : in out String;
      P : out Natural;
      Names : String;
      Indexes : Address);

   procedure Image_Enumeration_16 (
      Pos : Natural;
      S : in out String;
      P : out Natural;
      Names : String;
      Indexes : Address);

   procedure Image_Enumeration_32 (
      Pos : Natural;
      S : in out String;
      P : out Natural;
      Names : String;
      Indexes : Address);

end System.Img_Enum_New;
