pragma License (Unrestricted);
--  implementation unit
with System.Long_Long_Integer_Types;
private package Ada.Text_IO.Formatting is

   --  for Integer_IO
   procedure Integer_Image (
      To : out String;
      Last : out Natural;
      Item : System.Long_Long_Integer_Types.Word_Integer;
      Base : Number_Base);
   procedure Integer_Image (
      To : out String;
      Last : out Natural;
      Item : Long_Long_Integer;
      Base : Number_Base);

   --  for Modular_IO
   procedure Modular_Image (
      To : out String;
      Last : out Natural;
      Item : System.Long_Long_Integer_Types.Word_Unsigned;
      Base : Number_Base);
   procedure Modular_Image (
      To : out String;
      Last : out Natural;
      Item : System.Long_Long_Integer_Types.Long_Long_Unsigned;
      Base : Number_Base);

   --  for Integer_IO, Modular_IO, Float_IO, Fixed_IO
   function Get_Numeric_Literal (
      File : File_Type; -- Input_File_Type
      Real : Boolean)
      return String;

   --  for Complex_IO
   function Get_Complex_Literal (
      File : File_Type) -- Input_File_Type
      return String;

   --  for Enumeration_IO
   function Get_Enum_Literal (
      File : File_Type) -- Input_File_Type
      return String;

   --  for Get (..., Width);
   procedure Get_Field (
      File : File_Type; -- Input_File_Type
      Item : out String;
      Last : out Natural);

   --  put with layout
   procedure Head (
      File : File_Type; -- Output_File_Type
      Item : String;
      Width : Field);
   procedure Tail (
      File : File_Type; -- Output_File_Type
      Item : String;
      Width : Field);

   --  get from string
   procedure Get_Head (
      Item : String;
      First : out Positive;
      Last : out Natural);
   procedure Get_Tail (Item : String; First : out Positive);

   --  put to string
   procedure Head (Target : out String; Source : String);
   procedure Tail (Target : out String; Source : String);
   procedure Tail (Target : out Wide_String; Source : Wide_String);
   procedure Tail (Target : out Wide_Wide_String; Source : Wide_Wide_String);

end Ada.Text_IO.Formatting;
