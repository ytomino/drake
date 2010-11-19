package body Ada.Text_IO.Enumeration_IO is
   pragma Suppress (All_Checks);

   procedure Get (
      File : File_Type;
      Item : out Enum) is
   begin
      raise Program_Error;
   end Get;

   procedure Get (
      Item : out Enum) is
   begin
      raise Program_Error;
   end Get;

   procedure Put (
      File : File_Type;
      Item : Enum;
      Width : Field := Default_Width;
      Set : Type_Set := Default_Setting) is
   begin
      raise Program_Error;
   end Put;

   procedure Put (
      Item : Enum;
      Width : Field := Default_Width;
      Set : Type_Set := Default_Setting) is
   begin
      raise Program_Error;
   end Put;

   procedure Get (
      From : String;
      Item : out Enum;
      Last : out Positive) is
   begin
      raise Program_Error;
   end Get;

   procedure Put (
      To : out String;
      Item : Enum;
      Set : Type_Set := Default_Setting) is
   begin
      raise Program_Error;
   end Put;

end Ada.Text_IO.Enumeration_IO;
