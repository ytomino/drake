pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.Exception_Identification;
with Ada.IO_Exceptions;
with Ada.Streams;
with System.Native_Calendar;
with C.winbase;
with C.windef;
package System.Native_Directories is
   pragma Preelaborate;

   --  directory and file operations

   function Current_Directory return String;

   procedure Set_Directory (Directory : String);

   procedure Create_Directory (New_Directory : String);

   procedure Delete_Directory (Directory : String);

   procedure Delete_File (Name : String);

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean);

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean);

   procedure Replace_File (
      Source_Name : String;
      Target_Name : String);

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean);

   --  file and directory name operations

   function Full_Name (Name : String) return String;

   function Exists (Name : String) return Boolean;

   --  file and directory queries

   --  same as Ada.Directories.File_Kind
   type File_Kind is (Directory, Ordinary_File, Special_File);
   pragma Discard_Names (File_Kind);

   subtype Directory_Entry_Information_Type is
      C.winbase.WIN32_FILE_ATTRIBUTE_DATA;

   procedure Get_Information (
      Name : String;
      Information : aliased out Directory_Entry_Information_Type);

   function Kind (Attributes : C.windef.DWORD) return File_Kind;
   function Kind (Information : Directory_Entry_Information_Type)
      return File_Kind;

   function Size (Information : Directory_Entry_Information_Type)
      return Ada.Streams.Stream_Element_Count;

   function Modification_Time (Information : Directory_Entry_Information_Type)
      return Native_Calendar.Native_Time;

   procedure Set_Modification_Time (
      Name : String;
      Time : Native_Calendar.Native_Time);

   --  exceptions

   function IO_Exception_Id (Error : C.windef.DWORD)
      return Ada.Exception_Identification.Exception_Id;

   function Named_IO_Exception_Id (Error : C.windef.DWORD)
      return Ada.Exception_Identification.Exception_Id;

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception
      renames Ada.IO_Exceptions.Device_Error;

end System.Native_Directories;
