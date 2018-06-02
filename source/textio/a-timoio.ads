pragma License (Unrestricted);
--  separated and auto-loaded by compiler
private generic
   type Num is mod <>;
package Ada.Text_IO.Modular_IO is

   Default_Width : Field := Num'Width;
   Default_Base : Number_Base := 10;

   procedure Get (
      File : File_Type; -- Input_File_Type
      Item : out Num;
      Width : Field := 0);
   procedure Get (
      Item : out Num;
      Width : Field := 0);

   procedure Put (
      File : File_Type; -- Output_File_Type
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base);
   procedure Put (
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base);

   procedure Get (
      From : String;
      Item : out Num;
      Last : out Positive);
   procedure Put (
      To : out String;
      Item : Num;
      Base : Number_Base := Default_Base);

end Ada.Text_IO.Modular_IO;
