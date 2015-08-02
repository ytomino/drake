package body System.Img_Enum_New is

   procedure Image_Enumeration_8 (
      Pos : Natural;
      S : in out String;
      P : out Natural;
      Names : String;
      Indexes : Address)
   is
      type Index_Type is mod 2 ** 8;
      type Array_Type is array (Natural) of Index_Type;
      Indexes_2 : Array_Type;
      for Indexes_2'Address use Indexes;
      First : constant Natural := Natural (Indexes_2 (Pos));
      Next : constant Natural := Natural (Indexes_2 (Pos + 1));
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
      type Index_Type is mod 2 ** 16;
      type Array_Type is array (Natural) of Index_Type;
      Indexes_2 : Array_Type;
      for Indexes_2'Address use Indexes;
      First : constant Natural := Natural (Indexes_2 (Pos));
      Next : constant Natural := Natural (Indexes_2 (Pos + 1));
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
      type Index_Type is mod 2 ** 32;
      type Array_Type is array (Natural) of Index_Type;
      Indexes_2 : Array_Type;
      for Indexes_2'Address use Indexes;
      First : constant Natural := Natural (Indexes_2 (Pos));
      Next : constant Natural := Natural (Indexes_2 (Pos + 1));
   begin
      pragma Assert (S'Length >= Next - First);
      P := S'First - 1 + Next - First;
      S (S'First .. P) := Names (First .. Next - 1);
   end Image_Enumeration_32;

end System.Img_Enum_New;
