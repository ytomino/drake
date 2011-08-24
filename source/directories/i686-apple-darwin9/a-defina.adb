with Ada.Directories.Inside.File_Names;
function Ada.Directories.Equal_File_Names (Left, Right : String)
   return Boolean is
begin
   return Inside.File_Names.Equal_File_Names (Left, Right);
end Ada.Directories.Equal_File_Names;
