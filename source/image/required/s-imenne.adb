package body System.Img_Enum_New is

   procedure Image_Enumeration_8 (
      Pos : Natural;
      S : in out String;
      P : out Natural;
      Names : String;
      Indexes : Address)
   is
      pragma Suppress (Alignment_Check);
      type Index_Type is mod 2 ** 8;
      type Index_Array_Type is array (0 .. Pos + 1) of Index_Type;
      Indexes_All : Index_Array_Type;
      for Indexes_All'Address use Indexes;
      First : constant Natural := Natural (Indexes_All (Pos));
      Next : constant Natural := Natural (Indexes_All (Pos + 1));
   begin
      pragma Assert (S'Length >= Next - First);
      P := S'First - 1 + Next - First;
      S (S'First .. P) := Names (First .. Next - 1);
   end Image_Enumeration_8;

   procedure Image_Enumeration_16 (
      Pos : Natural;
      S : in out String;
      P : out Natural;
      Names : String;
      Indexes : Address)
   is
      pragma Suppress (Alignment_Check);
      type Index_Type is mod 2 ** 16;
      type Index_Array_Type is array (0 .. Pos + 1) of Index_Type;
      Indexes_All : Index_Array_Type;
      for Indexes_All'Address use Indexes;
      First : constant Natural := Natural (Indexes_All (Pos));
      Next : constant Natural := Natural (Indexes_All (Pos + 1));
   begin
      pragma Assert (S'Length >= Next - First);
      P := S'First - 1 + Next - First;
      S (S'First .. P) := Names (First .. Next - 1);
   end Image_Enumeration_16;

   procedure Image_Enumeration_32 (
      Pos : Natural;
      S : in out String;
      P : out Natural;
      Names : String;
      Indexes : Address)
   is
      pragma Suppress (Alignment_Check);
      type Index_Type is mod 2 ** 32;
      type Index_Array_Type is array (0 .. Pos + 1) of Index_Type;
      Indexes_All : Index_Array_Type;
      for Indexes_All'Address use Indexes;
      First : constant Natural := Natural (Indexes_All (Pos));
      Next : constant Natural := Natural (Indexes_All (Pos + 1));
   begin
      pragma Assert (S'Length >= Next - First);
      P := S'First - 1 + Next - First;
      S (S'First .. P) := Names (First .. Next - 1);
   end Image_Enumeration_32;

end System.Img_Enum_New;
