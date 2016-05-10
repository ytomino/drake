pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.Directories.Volumes;
package System.Native_Directories.File_Names is

   function Equal_File_Names (
      FS : Ada.Directories.Volumes.File_System;
      Left, Right : String)
      return Boolean;
   function Less_File_Names (
      FS : Ada.Directories.Volumes.File_System;
      Left, Right : String)
      return Boolean;

end System.Native_Directories.File_Names;
