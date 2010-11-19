pragma License (Unrestricted);
--  separated and auto-loaded by compiler
generic
   type Enum is (<>);
package Ada.Text_IO.Enumeration_IO is

   Default_Width : Field := 0;
   Default_Setting : Type_Set := Upper_Case;

   procedure Get (
      File : File_Type;
      Item : out Enum);
   procedure Get (
      Item : out Enum);

   procedure Put (
      File : File_Type;
      Item : Enum;
      Width : Field := Default_Width;
      Set : Type_Set := Default_Setting);
   procedure Put (
      Item : Enum;
      Width : Field := Default_Width;
      Set : Type_Set := Default_Setting);

   procedure Get (
      From : String;
      Item : out Enum;
      Last : out Positive);
   procedure Put (
      To : out String;
      Item : Enum;
      Set : Type_Set := Default_Setting);

end Ada.Text_IO.Enumeration_IO;
