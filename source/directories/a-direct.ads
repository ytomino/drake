pragma License (Unrestricted);
with Ada.Calendar;
with Ada.Hierarchical_File_Names;
with Ada.IO_Exceptions;
with Ada.Iterator_Interfaces;
with Ada.Streams;
private with Ada.Finalization;
private with System.Native_Directories.Searching;
package Ada.Directories is

   --  Directory and file operations:

   function Current_Directory return String;
   pragma Inline (Current_Directory); -- renamed

   procedure Set_Directory (Directory : String);
   pragma Inline (Set_Directory); -- renamed

   procedure Create_Directory (
      New_Directory : String;
      Form : String := "");
   pragma Inline (Create_Directory);

   procedure Delete_Directory (Directory : String);
   pragma Inline (Delete_Directory); -- renamed

   procedure Create_Path (
      New_Directory : String;
      Form : String := "");

   procedure Delete_Tree (Directory : String);

   procedure Delete_File (Name : String);
   pragma Inline (Delete_File); -- renamed

   --  modified
   --  These functions fail if Overwrite = False and New_Name already exists.
   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean := True); -- additional
   pragma Inline (Rename); -- renamed

   --  modified
   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Form : String); -- removed default
   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True);
   pragma Inline (Copy_File); -- renamed, or normal inline

   --  extended
   --  Overwrite a target file with another source file,
   --    and delete the source if it succeeded.
   --  Replace_File tries to preserve attributes of the target unlike Rename.
   procedure Replace_File (
      Source_Name : String;
      Target_Name : String);
   pragma Inline (Replace_File); -- renamed

   --  extended
   --  Create a symbolic link.
   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True);
   pragma Inline (Symbolic_Link); -- renamed

   --  File and directory name operations:

   function Full_Name (Name : String) return String;
   pragma Inline (Full_Name); -- renamed

   function Simple_Name (Name : String) return String
      renames Hierarchical_File_Names.Simple_Name;

   function Containing_Directory (Name : String) return String
      renames Hierarchical_File_Names.Containing_Directory;

   function Extension (Name : String) return String
      renames Hierarchical_File_Names.Extension;

   function Base_Name (Name : String) return String
      renames Hierarchical_File_Names.Base_Name;

   --  modified
   function Compose (
      Containing_Directory : String := "";
      Name : String;
      Extension : String := "";
      Path_Delimiter : Hierarchical_File_Names.Path_Delimiter_Type :=
         Hierarchical_File_Names.Default_Path_Delimiter) -- additional
      return String;
   pragma Inline (Compose);

--  type Name_Case_Kind is
--    (Unknown, Case_Sensitive, Case_Insensitive, Case_Preserving);

--  function Name_Case_Equivalence (Name : in String) return Name_Case_Kind;

   --  File and directory queries:

   type File_Kind is (Directory, Ordinary_File, Special_File);

   --  modified
   --  File_Size is essentially same as Stream_Element_Count.
--  type File_Size is range 0 .. implementation-defined;
   subtype File_Size is Streams.Stream_Element_Count;

   function Exists (Name : String) return Boolean;
   pragma Inline (Exists); -- renamed

   function Kind (Name : String) return File_Kind;

   function Size (Name : String) return File_Size;

   function Modification_Time (Name : String) return Calendar.Time;

   --  extended
   --  Set modification time of a file.
   procedure Set_Modification_Time (Name : String; Time : Calendar.Time);

   --  Directory searching:

   type Directory_Entry_Type is limited private;

--  subtype Assigned_Directory_Entry_Type is Directory_Entry_Type
--    with
--       Dynamic_Predicate => Is_Assigned (Assigned_Directory_Entry_Type),
--       Predicate_Failure => raise Status_Error;

   --  extended
   function Is_Assigned (Directory_Entry : Directory_Entry_Type)
      return Boolean;
   pragma Inline (Is_Assigned);

   type Filter_Type is array (File_Kind) of Boolean;
   pragma Pack (Filter_Type);

   type Search_Type is limited private;

--  subtype Open_Search_Type is Search_Type
--    with
--       Dynamic_Predicate => Is_Open (Open_Search_Type),
--       Predicate_Failure => raise Status_Error;

   --  extended
   function Is_Open (Search : Search_Type) return Boolean;
   pragma Inline (Is_Open);

   --  modified
   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String := "*"; -- additional default
      Filter : Filter_Type := (others => True));
   --  extended
   function Start_Search (
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True))
      return Search_Type;

   procedure End_Search (Search : in out Search_Type);

   function More_Entries (
      Search : Search_Type) -- Open_Search_Type
      return Boolean;

   procedure Get_Next_Entry (
      Search : in out Search_Type; -- Open_Search_Type
      Directory_Entry : out Directory_Entry_Type);
   --  extended
   --  The function version of Get_Next_Entry.
   function Get_Next_Entry (
      Search : aliased in out Search_Type) -- Open_Search_Type
      return Directory_Entry_Type;

   --  modified
   procedure Search (
      Directory : String;
      Pattern : String := "*"; -- additional default
      Filter : Filter_Type := (others => True);
      Process : not null access procedure (
         Directory_Entry : Directory_Entry_Type));

   --  extended from here
   --  AI12-0009-1, Directory Iteration

   type Directory_Listing is tagged limited private
      with
--       Constant_Indexing => Current_Entry,
         Constant_Indexing => Constant_Reference,
         Default_Iterator => Iterate,
         Iterator_Element => Directory_Entry_Type;
   pragma Preelaborable_Initialization (Directory_Listing);

--  subtype Open_Directory_Listing is Directory_Listing
--    with
--       Dynamic_Predicate => Is_Open (Open_Directory_Listing),
--       Predicate_Failure => raise Status_Error;

   function Is_Open (Listing : Directory_Listing) return Boolean; -- additional
   pragma Inline (Is_Open);

   function Entries (
      Directory : String;
      Pattern : String := "*"; -- additional default
      Filter : Filter_Type := (others => True))
      return Directory_Listing;

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   function Has_Entry (Position : Cursor) return Boolean;
   pragma Inline (Has_Entry);

   package Directory_Iterators is
      new Iterator_Interfaces (Cursor, Has_Entry);

   --  Note: To be consistent with the existing instances in Ada.Containers,
   --    like Vector/List/Set/Map_Iterator_Interfaces, should it be renamed to
   --    Directory_Listing_Iterator_Interfaces?

   function Iterate (
      Listing : Directory_Listing'Class) -- Open_Directory_Listing'Class
      return Directory_Iterators.Forward_Iterator'Class;

   function Current_Entry (
      Entries : Directory_Listing'Class; -- Open_Directory_Listing'Class
      Position : Cursor)
      return Directory_Entry_Type;
--    with Pre => Has_Entry (Position);

   type Constant_Reference_Type (
      Element : not null access constant Directory_Entry_Type) is null record
      with Implicit_Dereference => Element;
      --  additional

   function Constant_Reference (
      Container : aliased Directory_Listing; -- Open_Directory_Listing
      Position : Cursor)
      return Constant_Reference_Type;
      --  additional
   pragma Inline (Constant_Reference);

   --  to here

   --  Operations on Directory Entries:

   --  extended
   --  Get Directory_Entry_Type of one file to get plural information.
   procedure Get_Entry (
      Name : String;
      Directory_Entry : out Directory_Entry_Type);
   function Get_Entry (
      Name : String)
      return Directory_Entry_Type;

   function Simple_Name (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return String;

   function Full_Name (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return String;

   function Kind (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return File_Kind;

   function Size (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return File_Size;

   function Modification_Time (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Calendar.Time;

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   Device_Error : exception
      renames IO_Exceptions.Device_Error;

private

   --  directory and file operations

   function Current_Directory return String
      renames System.Native_Directories.Current_Directory;

   procedure Set_Directory (Directory : String)
      renames System.Native_Directories.Set_Directory;

   procedure Delete_Directory (Directory : String)
      renames System.Native_Directories.Delete_Directory;

   procedure Delete_File (Name : String)
      renames System.Native_Directories.Delete_File;

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean := True)
      renames System.Native_Directories.Rename;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True)
      renames System.Native_Directories.Copy_File;

   procedure Replace_File (
      Source_Name : String;
      Target_Name : String)
      renames System.Native_Directories.Replace_File;

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True)
      renames System.Native_Directories.Symbolic_Link;

   --  file and directory name operations

   function Full_Name (Name : String) return String
      renames System.Native_Directories.Full_Name;

   --  file and directory queries

   function Exists (Name : String) return Boolean
      renames System.Native_Directories.Exists;

   --  directory searching

   type String_Access is access String;

   type Search_Access is access Search_Type;
   for Search_Access'Storage_Size use 0;

   type Directory_Entry_Status is (Empty, Attached, Detached);
   pragma Discard_Names (Directory_Entry_Status);

   type Non_Controlled_Directory_Entry_Type is record
      Path : String_Access;
      Directory_Entry : aliased
         System.Native_Directories.Searching.Directory_Entry_Access;
      Additional : aliased
         System.Native_Directories.Searching.Directory_Entry_Additional_Type;
      Status : Directory_Entry_Status := Empty;
   end record;

   package Controlled is

      type Directory_Entry_Type is limited private;

      function Reference (Object : Directories.Directory_Entry_Type)
         return not null access Non_Controlled_Directory_Entry_Type;
      pragma Inline (Reference);

   private

      type Directory_Entry_Type is
         limited new Finalization.Limited_Controlled with
      record
         Data : aliased Non_Controlled_Directory_Entry_Type;
      end record;

      overriding procedure Finalize (Object : in out Directory_Entry_Type);

   end Controlled;

   type Directory_Entry_Type is new Controlled.Directory_Entry_Type;

   type Search_Type is limited new Finalization.Limited_Controlled with record
      Search : aliased System.Native_Directories.Searching.Search_Type := (
         Handle => System.Native_Directories.Searching.Null_Handle,
         others => <>);
      Path : String_Access;
      Next_Directory_Entry : aliased Directory_Entry_Type;
      Next_Is_Queried : Boolean;
      Count : Natural;
   end record;

   overriding procedure Finalize (Search : in out Search_Type);

   --  Directory Iteration

   type Directory_Listing is tagged limited record
      Search : aliased Search_Type;
   end record;

   type Cursor is new Natural;

   type Directory_Iterator is
      new Directory_Iterators.Forward_Iterator with
   record
      Search : Search_Access;
   end record;

   overriding function First (Object : Directory_Iterator) return Cursor;
   overriding function Next (Object : Directory_Iterator; Position : Cursor)
      return Cursor;

end Ada.Directories;
