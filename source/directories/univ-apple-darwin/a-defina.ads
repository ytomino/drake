pragma License (Unrestricted);
--  extended unit
function Ada.Directories.Equal_File_Names (Left, Right : String)
   return Boolean;
--  This function compare two file names by the method of the file system.
--  For example, it uses NFD and case-insensitive on HFS+.
