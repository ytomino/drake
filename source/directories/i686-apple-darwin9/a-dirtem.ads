pragma License (Unrestricted);
--  extended package for temporary files
with Ada.IO_Exceptions;
package Ada.Directories.Temporary is

   function Temporary_Directory return String;

   function Create_Temporary_File (
      Directory : String := Temporary_Directory)
      return String;

   function Create_Temporary_Directory (
      Directory : String := Temporary_Directory)
      return String;

   Use_Error : exception renames IO_Exceptions.Use_Error;

end Ada.Directories.Temporary;
