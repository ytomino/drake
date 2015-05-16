pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.IO_Exceptions;
package System.Native_Temporary_Files is
   --  There are subprograms to create temporary file or directory.
   pragma Preelaborate;

   function Temporary_Directory return String;
   procedure Set_Temporary_Directory (Name : String);

   function Create_Temporary_File (
      Directory : String)
      return String;

   function Create_Temporary_Directory (
      Directory : String)
      return String;

   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;

end System.Native_Temporary_Files;
