pragma License (Unrestricted);
with Ada.IO_Exceptions;
with Ada.IO_Modes;
with Ada.Streams.Stream_IO;
generic
   type Element_Type (<>) is private;
package Ada.Sequential_IO is

   type File_Type is limited private;

--  type File_Mode is (In_File, Out_File, Append_File);
   type File_Mode is new IO_Modes.File_Mode; -- for conversion

   --  File management

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "");

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");

   procedure Close (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset (File : in out File_Type; Mode : File_Mode);
   procedure Reset (File : in out File_Type);

   function Mode (File : File_Type) return File_Mode;
   pragma Inline (Mode);
   function Name (File : File_Type) return String;
   pragma Inline (Name);
   function Form (File : File_Type) return String;
   pragma Inline (Form);

   function Is_Open (File : File_Type) return Boolean;
   pragma Inline (Is_Open);

   --  Input and output operations

   procedure Read (File : File_Type; Item : out Element_Type);
   procedure Write (File : File_Type; Item : Element_Type);

   function End_Of_File (File : File_Type) return Boolean;

   --  Exceptions

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Mode_Error : exception
      renames IO_Exceptions.Mode_Error;
   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   Device_Error : exception
      renames IO_Exceptions.Device_Error;
   End_Error : exception
      renames IO_Exceptions.End_Error;
   Data_Error : exception
      renames IO_Exceptions.Data_Error;

private

   type File_Type is new Streams.Stream_IO.File_Type;

end Ada.Sequential_IO;
