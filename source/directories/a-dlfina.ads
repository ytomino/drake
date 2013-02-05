pragma License (Unrestricted);
--  extended unit
with Ada.Directories.Information;
function Ada.Directories.Less_File_Names (
   FS : Information.File_System;
   Left, Right : String)
   return Boolean;
--  This function is "<" version of Ada.Directories.Equal_File_Names.
