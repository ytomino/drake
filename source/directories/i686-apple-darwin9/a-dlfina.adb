with Ada.Directories.Inside.File_Names;
function Ada.Directories.Less_File_Names (Left, Right : String)
   return Boolean is
begin
   return Inside.File_Names.Less_File_Names (Left, Right);
end Ada.Directories.Less_File_Names;
