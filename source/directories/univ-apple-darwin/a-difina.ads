pragma License (Unrestricted);
--  implementation unit
with Ada.Directories.Volumes;
package Ada.Directories.Inside.File_Names is

   --  compare file names with normalization and case-insensitive, if HFS+
   function Equal_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean;
   function Less_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean;

end Ada.Directories.Inside.File_Names;
