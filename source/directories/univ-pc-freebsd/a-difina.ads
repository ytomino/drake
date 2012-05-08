pragma License (Unrestricted);
--  implementation unit
with Ada.Directories.Inside.File_Systems;
package Ada.Directories.Inside.File_Names is

   function Equal_File_Names (
      FS : File_Systems.File_System;
      Left, Right : String)
      return Boolean;
   function Less_File_Names (
      FS : File_Systems.File_System;
      Left, Right : String)
      return Boolean;

end Ada.Directories.Inside.File_Names;
