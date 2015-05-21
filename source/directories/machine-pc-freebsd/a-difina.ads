pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD (or Linux)
with Ada.Directories.Volumes;
private package Ada.Directories.File_Names is

   function Equal_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean;
   function Less_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean;

end Ada.Directories.File_Names;
