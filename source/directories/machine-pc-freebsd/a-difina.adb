package body Ada.Directories.File_Names is

   --  implementation

   function Equal_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean
   is
      pragma Unreferenced (FS);
   begin
      return Left = Right;
   end Equal_File_Names;

   function Less_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean
   is
      pragma Unreferenced (FS);
   begin
      return Left < Right;
   end Less_File_Names;

end Ada.Directories.File_Names;
