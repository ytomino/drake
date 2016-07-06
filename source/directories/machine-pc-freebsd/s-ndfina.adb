package body System.Native_Directories.File_Names is

   --  implementation

   function Equal_File_Names (
      FS : Ada.Directories.Volumes.File_System;
      Left, Right : String)
      return Boolean
   is
      pragma Unreferenced (FS);
   begin
      return Left = Right;
   end Equal_File_Names;

   function Less_File_Names (
      FS : Ada.Directories.Volumes.File_System;
      Left, Right : String)
      return Boolean
   is
      pragma Unreferenced (FS);
   begin
      return Left < Right;
   end Less_File_Names;

end System.Native_Directories.File_Names;
