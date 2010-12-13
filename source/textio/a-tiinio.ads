pragma License (Unrestricted);
--  separated and auto-loaded by compiler
generic
   type Num is range <>;
package Ada.Text_IO.Integer_IO is

   Default_Width : Field := Num'Width;
   Default_Base : Number_Base := 10;

   procedure Get (
      File : File_Type;
      Item : out Num;
      Width : Field := 0);
   procedure Get (
      Item : out Num;
      Width : Field := 0);
   --  extended
   procedure Get (
      File : not null File_Access;
      Item : out Num;
      Width : Field := 0);

   procedure Put (
      File : File_Type;
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base);
   procedure Put (
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base);
   --  extended
   procedure Put (
      File : not null File_Access;
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

end Ada.Text_IO.Integer_IO;
