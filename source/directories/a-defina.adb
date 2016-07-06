with System.Native_Directories.File_Names;
function Ada.Directories.Equal_File_Names (
   FS : Volumes.File_System;
   Left, Right : String)
   return Boolean is
begin
   return System.Native_Directories.File_Names.Equal_File_Names (
      FS,
      Left,
      Right);
end Ada.Directories.Equal_File_Names;
