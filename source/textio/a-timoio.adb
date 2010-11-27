package body Ada.Text_IO.Modular_IO is
   pragma Suppress (All_Checks);

   procedure Get (
      File : File_Type;
      Item : out Num;
      Width : Field := 0) is
   begin
      raise Program_Error;
   end Get;

   procedure Get (
      Item : out Num;
      Width : Field := 0) is
   begin
      raise Program_Error;
   end Get;

   procedure Put (
      File : File_Type;
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base) is
   begin
      raise Program_Error;
   end Put;

   procedure Put (
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base) is
   begin
      raise Program_Error;
   end Put;

   procedure Get (
      From : String;
      Item : out Num;
      Last : out Positive) is
   begin
      raise Program_Error;
   end Get;

   procedure Put (
      To : out String;
      Item : Num;
      Base : Number_Base := Default_Base) is
   begin
      raise Program_Error;
   end Put;

end Ada.Text_IO.Modular_IO;
