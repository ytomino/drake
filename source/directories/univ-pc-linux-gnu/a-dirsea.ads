pragma License (Unrestricted);
--  implementation unit for Ada.Directories
with Ada.IO_Exceptions;
with C.dirent;
with C.stdint;
with C.sys.stat;
package Ada.Directory_Searching is

   subtype Directory_Entry_Information_Type is C.sys.stat.struct_stat64;

   subtype Directory_Entry_Type is C.dirent.struct_dirent64;

   --  same as Directories.File_Kind
   type File_Kind is (Directory, Ordinary_File, Special_File);
   pragma Discard_Names (File_Kind);
   --  same as Directories.Filter_Type
   type Filter_Type is array (File_Kind) of Boolean;
   pragma Pack (Filter_Type);
   pragma Suppress_Initialization (Filter_Type);

   subtype Handle_Type is C.dirent.DIR_ptr;

   type Search_Type is record
      Handle : C.dirent.DIR_ptr;
      Pattern : C.char_ptr;
      Filter : C.stdint.uint16_t; -- bit set of DT_xxx
   end record;
   pragma Suppress_Initialization (Search_Type);

   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String;
      Filter : Filter_Type);

   procedure End_Search (Search : in out Search_Type);

   procedure Get_Next_Entry (
      Search : in out Search_Type;
      Directory_Entry : not null access Directory_Entry_Type;
      Has_Next_Entry : out Boolean);

   function Simple_Name (Directory_Entry : Directory_Entry_Type)
      return String;

   procedure Get_Information (
      Directory : String;
      Directory_Entry : Directory_Entry_Type;
      Information : not null access Directory_Entry_Information_Type);

   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;

   --  for Ada.Directories

   function lstat (
      path : access constant C.char;
      buf : access C.sys.stat.struct_stat64)
      return C.signed_int
      renames C.sys.stat.lstat64;

   O_EXLOCK : constant := 0;

end Ada.Directory_Searching;
