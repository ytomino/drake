pragma License (Unrestricted);
with Ada.IO_Exceptions;
with Ada.Calendar;
private with Ada.Finalization;
private with C.dirent;
private with C.sys.dirent;
private with C.sys.stat;
package Ada.Directories is

   --  Directory and file operations:

   function Current_Directory return String;

   procedure Set_Directory (Directory : String);

   procedure Create_Directory (
      New_Directory : String;
      Form : String := "");

   procedure Delete_Directory (Directory : String);

   procedure Create_Path (
      New_Directory : String;
      Form : String := "");

   procedure Delete_Tree (Directory : String);

   procedure Delete_File (Name : String);

   procedure Rename (Old_Name, New_Name : String);

   procedure Copy_File (
      Source_Name,
      Target_Name : String;
      Form : String := "");

   --  File and directory name operations:

   function Full_Name (Name : String) return String;

   function Simple_Name (Name : String) return String;

   function Containing_Directory (Name : String) return String;

   function Extension (Name : String) return String;

   function Base_Name (Name : String) return String;

   function Compose (
      Containing_Directory : String := "";
      Name : String;
      Extension : String := "")
      return String;

   --  File and directory queries:

   type File_Kind is (Directory, Ordinary_File, Special_File);

--  type File_Size is range 0 .. implementation-defined;
   type File_Size is mod 2 ** 64;

   function Exists (Name : String) return Boolean;

   function Kind (Name : String) return File_Kind;

   function Size (Name : String) return File_Size;

   function Modification_Time (Name : String) return Calendar.Time;

   --  Directory searching:

   type Directory_Entry_Type is limited private;

   type Filter_Type is array (File_Kind) of Boolean;

   type Search_Type is limited private;

   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String;
      Filter : Filter_Type := (others => True));

   --  extended
   function Start_Search (
      Directory : String;
      Pattern : String;
      Filter : Filter_Type := (others => True))
      return Search_Type;

   procedure End_Search (Search : in out Search_Type);

   function More_Entries (Search : Search_Type) return Boolean;

   procedure Get_Next_Entry (
      Search : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type);

   procedure Search (
      Directory : String;
      Pattern : String;
      Filter : Filter_Type := (others => True);
      Process : not null access procedure (
         Directory_Entry : Directory_Entry_Type));

   --  Operations on Directory Entries:

   function Simple_Name (Directory_Entry : Directory_Entry_Type)
      return String;

   function Full_Name (Directory_Entry : Directory_Entry_Type)
      return String;

   function Kind (Directory_Entry : Directory_Entry_Type)
      return File_Kind;

   function Size (Directory_Entry : Directory_Entry_Type)
      return File_Size;

   function Modification_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time;

   Status_Error : exception renames IO_Exceptions.Status_Error;
   Name_Error : exception renames IO_Exceptions.Name_Error;
   Use_Error : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;

private

   type Directory_Entry_Type is limited record
      Search : access Search_Type := null;
      Entry_Data : aliased C.sys.dirent.struct_dirent;
      State_Data : aliased C.sys.stat.struct_stat;
   end record;

   type String_Access is access String;

   type Search_Type is new Finalization.Limited_Controlled with record
      Handle : C.dirent.DIR_ptr := null;
      Path : String_Access;
      Pattern : C.char_ptr;
      Filter : Filter_Type;
      Has_Next : Boolean;
      Data : aliased C.sys.dirent.struct_dirent;
   end record;

   overriding procedure Finalize (Search : in out Search_Type);
   procedure End_Search (Search : in out Search_Type) renames Finalize;

   --  for Ada.Directories.Information
   procedure Check_Assigned (Directory_Entry : Directory_Entry_Type);
   procedure Get_Attributes (
      Name : String;
      Attributes : out C.sys.stat.struct_stat);

   --  for Ada.Directories.Temporary
   procedure Include_Trailing_Path_Delimiter (
      S : in out String;
      Last : in out Natural);

   --  local subprogram
   procedure Exclude_Trailing_Path_Delimiter (
      S : String;
      Last : in out Natural);

end Ada.Directories;
