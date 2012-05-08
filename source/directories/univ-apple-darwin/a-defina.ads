pragma License (Unrestricted);
--  extended unit
with Ada.Directories.Information;
function Ada.Directories.Equal_File_Names (
   FS : Information.File_System;
   Left, Right : String)
   return Boolean;
--  This function compare two file names by the method of the file system.
--  For example, it uses NFD and case-insensitive on HFS+.
