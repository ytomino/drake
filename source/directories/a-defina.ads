pragma License (Unrestricted);
--  extended unit
with Ada.Directories.Volumes;
function Ada.Directories.Equal_File_Names (
   FS : Volumes.File_System;
   Left, Right : String)
   return Boolean;
pragma Inline (Ada.Directories.Equal_File_Names);
--  This function compare two file names by the method of the file system.
--  For example, it uses NFD and case-insensitive on HFS+.
