with Ada.Directories.Inside.File_Names;
with Ada.Directories.Inside.File_Systems;
function Ada.Directories.Equal_File_Names (
   FS : Information.File_System;
   Left, Right : String)
   return Boolean
is
   Inside_FS : Inside.File_Systems.File_System;
   for Inside_FS'Address use FS'Address;
begin
   return Inside.File_Names.Equal_File_Names (Inside_FS, Left, Right);
end Ada.Directories.Equal_File_Names;
