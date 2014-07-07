pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with C.termios;
package System.Native_IO.Text_IO is
   pragma Preelaborate;

   subtype Buffer_Type is String (1 .. 6); -- one code-point of UTF-8

   --  terminal

   procedure Terminal_Size (
      Handle : Handle_Type;
      Line_Length, Page_Length : out Natural);
   procedure Set_Terminal_Size (
      Handle : Handle_Type;
      Line_Length, Page_Length : Natural);

   procedure Terminal_View (
      Handle : Handle_Type;
      Left, Top : out Positive;
      Right, Bottom : out Natural);

   procedure Terminal_Position (
      Handle : Handle_Type;
      Col, Line : out Positive);
   procedure Set_Terminal_Position (
      Handle : Handle_Type;
      Col, Line : Positive);
   procedure Set_Terminal_Col (
      Handle : Handle_Type;
      To : Positive);

   procedure Terminal_Clear (
      Handle : Handle_Type);

   subtype Setting is C.termios.struct_termios;

   procedure Set_Non_Canonical_Mode (
      Handle : Handle_Type;
      Wait : Boolean;
      Saved_Settings : aliased out Setting);

   procedure Restore (
      Handle : Handle_Type;
      Settings : aliased Setting);

   --  exceptions

   Data_Error : exception
      renames Ada.IO_Exceptions.Data_Error;

end System.Native_IO.Text_IO;
