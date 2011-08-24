pragma License (Unrestricted);
--  implementation unit
package Ada.Directories.Inside.File_Names is

   --  compare file names with normalization and case-insensitive
   function Equal_File_Names (Left, Right : String) return Boolean;
   function Less_File_Names (Left, Right : String) return Boolean;

end Ada.Directories.Inside.File_Names;
