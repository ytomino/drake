with System.Native_Directories.File_Names;
function Ada.Directories.Less_File_Names (
   FS : Volumes.File_System;
   Left, Right : String)
   return Boolean is
begin
   return System.Native_Directories.File_Names.Less_File_Names (
      FS,
      Left,
      Right);
end Ada.Directories.Less_File_Names;
