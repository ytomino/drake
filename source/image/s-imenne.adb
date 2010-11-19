package body System.Img_Enum_New is
   pragma Suppress (All_Checks);

   procedure Image_Enumeration_8 (
      Pos : Natural;
      S : in out String;
      P : out Natural;
      Names : String;
      Indexes : Address) is
   begin
      raise Program_Error;
   end Image_Enumeration_8;

   procedure Image_Enumeration_16 (
      Pos : Natural;
      S : in out String;
      P : out Natural;
      Names : String;
      Indexes : Address) is
   begin
      raise Program_Error;
   end Image_Enumeration_16;

end System.Img_Enum_New;
