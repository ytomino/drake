pragma License (Unrestricted);
with Ada.IO_Exceptions;
with Ada.Calendar;
with Ada.Hierarchical_File_Names;
with Ada.Iterator_Interfaces;
with Ada.Streams;
private with Ada.Directory_Searching;
private with Ada.Finalization;
package Ada.Directories is

   --  Directory and file operations:

   function Current_Directory return String;
   pragma Inline (Current_Directory); -- renamed

   procedure Set_Directory (Directory : String);
   pragma Inline (Set_Directory); -- renamed

   procedure Create_Directory (
      New_Directory : String;
      Form : String := "");
   pragma Inline (Create_Directory); -- renamed

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
   pragma Inline (Copy_File); -- renamed

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

   --  modified
   --  It's specified that Containing_Directory raises Use_Error
   --    when Name is a simple name.
   --  If the additional parameter Raise_On_Error = False,
   --    Containing_Directory returns null string when Name is a simple name.
   function Containing_Directory (
      Name : String;
      Raise_On_Error : Boolean := True) -- additional
      return String
      renames Hierarchical_File_Names.Containing_Directory;

   function Extension (Name : String) return String
      renames Hierarchical_File_Names.Extension;

   function Base_Name (Name : String) return String
      renames Hierarchical_File_Names.Base_Name;

   --  extended
   --  There are procedure version.
   procedure Simple_Name (
      Name : String;
      First : out Positive;
      Last : out Natural)
      renames Hierarchical_File_Names.Simple_Name;
   procedure Containing_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural)
      renames Hierarchical_File_Names.Containing_Directory;
   procedure Extension (
      Name : String;
      First : out Positive;
      Last : out Natural)
      renames Hierarchical_File_Names.Extension;
   procedure Base_Name (
      Name : String;
      First : out Positive;
      Last : out Natural)
      renames Hierarchical_File_Names.Base_Name;

   function Compose (
      Containing_Directory : String := "";
      Name : String;
      Extension : String := "")
      return String
      renames Hierarchical_File_Names.Compose_No_Folding;

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

   type Filter_Type is array (File_Kind) of Boolean;
   pragma Pack (Filter_Type);

   --  modified
   --  Search_Type has its iterator. See below.
--  type Search_Type is limited private;
   type Search_Type is tagged limited private
      with
         Constant_Indexing => Constant_Reference,
         Default_Iterator => Iterate,
         Iterator_Element => Directory_Entry_Type;

   --  modified
   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String := "*"; -- additional default
      Filter : Filter_Type := (others => True));

   --  extended
   --  This function version Start_Search enables to write
   --    "for E of Start_Search (...) loop".
   function Start_Search (
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True))
      return Search_Type;

   procedure End_Search (Search : in out Search_Type);

   function More_Entries (Search : Search_Type) return Boolean;

   procedure Get_Next_Entry (
      Search : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type);

   --  modified
   procedure Search (
      Directory : String;
      Pattern : String := "*"; -- additional default
      Filter : Filter_Type := (others => True);
      Process : not null access procedure (
         Directory_Entry : Directory_Entry_Type));

   --  extended
   --  There is an iterator for AI12-0009-1 (?)
   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);
   function Has_Element (Position : Cursor) return Boolean;
   pragma Inline (Has_Element);
   function Element (Container : Search_Type'Class; Position : Cursor)
      return Directory_Entry_Type;
   type Constant_Reference_Type (
      Element : not null access constant Directory_Entry_Type) is null record
      with Implicit_Dereference => Element;
   function Constant_Reference (
      Container : aliased Search_Type;
      Position : Cursor)
      return Constant_Reference_Type;
   pragma Inline (Constant_Reference);
   package Search_Iterator_Interfaces is
      new Iterator_Interfaces (Cursor, Has_Element);
   function Iterate (Container : Search_Type)
      return Search_Iterator_Interfaces.Forward_Iterator'Class;

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

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   Device_Error : exception
      renames IO_Exceptions.Device_Error;

private

   type String_Access is access String;

   type Search_Access is access Search_Type;
   for Search_Access'Storage_Size use 0;

   type Directory_Entry_Status is (Empty, Attached, Detached);
   pragma Discard_Names (Directory_Entry_Status);

   type Non_Controlled_Directory_Entry_Type is record
      Path : String_Access;
      Directory_Entry : Directory_Searching.Directory_Entry_Access;
      Additional : aliased Directory_Searching.Directory_Entry_Additional_Type;
      Status : Directory_Entry_Status := Empty;
   end record;

   package Controlled is

      type Directory_Entry_Type is limited private;

      function Reference (Object : Directory_Entry_Type)
         return not null access Non_Controlled_Directory_Entry_Type;
      pragma Inline (Reference);

   private

      type Directory_Entry_Type is
         new Finalization.Limited_Controlled with
      record
         Data : aliased Non_Controlled_Directory_Entry_Type;
      end record;

      overriding procedure Finalize (Object : in out Directory_Entry_Type);

   end Controlled;

   type Directory_Entry_Type is new Controlled.Directory_Entry_Type;

   type Search_Type is new Finalization.Limited_Controlled with record
      Search : aliased Directory_Searching.Search_Type := (
         Handle => Directory_Searching.Null_Handle,
         others => <>);
      Path : String_Access;
      Next_Directory_Entry : aliased Directory_Entry_Type;
      Next_Is_Queried : Boolean;
      Count : Natural;
   end record;

   overriding procedure Finalize (Search : in out Search_Type);

   type Cursor is new Natural;

   type Search_Iterator is new Search_Iterator_Interfaces.Forward_Iterator
      with
   record
      Search : Search_Access;
   end record;

   overriding function First (Object : Search_Iterator) return Cursor;
   overriding function Next (Object : Search_Iterator; Position : Cursor)
      return Cursor;

   package Streaming is

      procedure Missing_Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Search_Iterator);
      function Missing_Input (
         Stream : not null access Streams.Root_Stream_Type'Class)
         return Search_Iterator;
      procedure Missing_Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Search_Iterator);

      pragma Import (Ada, Missing_Read, "__drake_program_error");
      pragma Import (Ada, Missing_Input, "__drake_program_error");
      pragma Import (Ada, Missing_Write, "__drake_program_error");

   end Streaming;

   for Search_Iterator'Read use Streaming.Missing_Read;
   for Search_Iterator'Input use Streaming.Missing_Input;
   for Search_Iterator'Write use Streaming.Missing_Write;
   for Search_Iterator'Output use Streaming.Missing_Write;

end Ada.Directories;
