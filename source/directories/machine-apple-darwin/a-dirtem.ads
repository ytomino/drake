pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
package Ada.Directories.Temporary is
   --  There are subprograms to create temporary file or directory.

   --  settings

   function Temporary_Directory return String;
   procedure Set_Temporary_Directory (Name : String);

   --  creating new entries

   function Create_Temporary_File (
      Directory : String := Temporary_Directory)
      return String;

   function Create_Temporary_Directory (
      Directory : String := Temporary_Directory)
      return String;

   --  exceptions

   Use_Error : exception
      renames IO_Exceptions.Use_Error;

end Ada.Directories.Temporary;
