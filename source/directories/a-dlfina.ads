pragma License (Unrestricted);
--  extended unit
with Ada.Directories.Volumes;
function Ada.Directories.Less_File_Names (
   FS : Volumes.File_System;
   Left, Right : String)
   return Boolean;
pragma Inline (Ada.Directories.Less_File_Names);
--  This function is "<" version of Ada.Directories.Equal_File_Names.
