pragma License (Unrestricted);
--  implementation unit for Ada.Directories
with Ada.IO_Exceptions;
with System;
with C.winbase;
with C.winnt;
package Ada.Directory_Searching is

   subtype Directory_Entry_Information_Type is
      C.winbase.WIN32_FILE_ATTRIBUTE_DATA;

   subtype Directory_Entry_Type is C.winbase.WIN32_FIND_DATA;

   --  same as Directories.File_Kind
   type File_Kind is (Directory, Ordinary_File, Special_File);
   pragma Discard_Names (File_Kind);
   --  same as Directories.Filter_Type
   type Filter_Type is array (File_Kind) of Boolean;
   pragma Pack (Filter_Type);
   pragma Suppress_Initialization (Filter_Type);

   subtype Handle_Type is C.winnt.HANDLE;

   Null_Handle : constant Handle_Type := Handle_Type (System.Null_Address);

   type Search_Type is record
      Handle : C.winnt.HANDLE;
      Filter : Filter_Type;
   end record;
   pragma Suppress_Initialization (Search_Type);

   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String;
      Filter : Filter_Type;
      Directory_Entry : not null access Directory_Entry_Type;
      Has_Next_Entry : out Boolean);

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

   function Kind (Information : Directory_Entry_Information_Type)
      return File_Kind;

   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;

end Ada.Directory_Searching;
