pragma License (Unrestricted);
--  implementation package
package Ada.Text_IO.Inside.Formatting is

   --  for Integer_IO, Modular_IO, Float_IO, Fixed_IO
   function Get_Numeric_Literal (File : File_Type; Real : Boolean)
      return String;

   --  for Enumeration_IO
   function Get_Enum_Literal (File : File_Type) return String;

   --  for Get (..., Width);
   procedure Get_Field (
      File : File_Type;
      Item : out String;
      Last : out Natural);

   --  put with layout
   procedure Head (File : File_Type; Item : String; Width : Field);
   procedure Tail (File : File_Type; Item : String; Width : Field);

   --  get from string
   procedure Get_Head (
      Item : String;
      First : out Positive;
      Last : out Natural);
   procedure Get_Tail (Item : String; First : out Positive);

   --  put to string
   procedure Head (
      Target : out String;
      Source : String;
      Padding : Character := ' ');
   procedure Tail (
      Target : out String;
      Source : String;
      Padding : Character := ' ');

end Ada.Text_IO.Inside.Formatting;
