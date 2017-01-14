pragma License (Unrestricted);
--  extended unit specialized for Windows
with Ada.IO_Exceptions;
package Ada.Hierarchical_File_Names is
   --  This package is "Pure" version of
   --    Ada.Directories.Hierarchical_File_Names.
   pragma Pure;

   --  path delimiter

   subtype Path_Delimiter_Type is Character;
--    with Static_Predicate => Path_Delimiter in '/' | '\';

   Default_Path_Delimiter : constant Character := '\';

   function Is_Path_Delimiter (Item : Character) return Boolean;
   pragma Inline (Is_Path_Delimiter);

   procedure Include_Trailing_Path_Delimiter (
      S : in out String;
      Last : in out Natural;
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter);
   procedure Exclude_Trailing_Path_Delimiter (
      S : String;
      Last : in out Natural);

   --  operations in Ada.Directories

   function Simple_Name (Name : String) return String;
   --  extended
   --  This function returns null string instead of Name_Error,
   --    if Name has no simple name part.
   function Unchecked_Simple_Name (Name : String) return String;

   function Containing_Directory (Name : String) return String;
   --  extended
   --  This function returns null string instead of Use_Error,
   --     if Name has no directory part.
   function Unchecked_Containing_Directory (Name : String) return String;

   function Extension (Name : String) return String;

   function Base_Name (Name : String) return String;

   --  extended
   --  There are procedure version.
   procedure Simple_Name (
      Name : String;
      First : out Positive;
      Last : out Natural);
   procedure Containing_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural);
   procedure Extension (
      Name : String;
      First : out Positive;
      Last : out Natural);
   procedure Base_Name (
      Name : String;
      First : out Positive;
      Last : out Natural);

   --  operations in Ada.Directories.Hierarchical_File_Names

   function Is_Simple_Name (Name : String) return Boolean;

   function Is_Root_Directory_Name (Name : String) return Boolean;

   function Is_Parent_Directory_Name (Name : String) return Boolean;

   function Is_Current_Directory_Name (Name : String) return Boolean;

   function Is_Full_Name (Name : String) return Boolean;

   function Is_Relative_Name (Name : String) return Boolean;

--  function Simple_Name (Name : String) return String
--    renames Directories.Simple_Name;

--  function Containing_Directory (Name : String) return String
--     renames Directories.Containing_Directory;

   function Initial_Directory (Name : String) return String;

   function Relative_Name (Name : String) return String;
   --  extended
   --  This function returns null string instead of Name_Error,
   --    if Name has no directory part.
   function Unchecked_Relative_Name (Name : String) return String;

   --  extended
   --  There are procedure version.
   procedure Initial_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural);
   procedure Relative_Name (
      Name : String;
      First : out Positive;
      Last : out Natural);

   function Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "";
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String;

   --  extended
   --  This function folds current/parent directory names.
   --  For example: Normalized_Compose ("A/B", "../C") = "A/C".
   function Normalized_Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "";
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String;

   --  extended
   --  This function returns the relative name from the base directory.
   --  For example: Relative_Name ("A", "B") = "../A",
   --    Relative_Name (Name, Initial_Directory (Name)) = Relative_Name (Name)
   function Relative_Name (
      Name : String;
      From : String;
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String;

   --  extended
   --  This is a "folded" version of Containing_Directory if Directory /= "".
   --    Otherwise, it returns ".." as the parent directory name.
   --  For example: Parent_Directory ("A/B/.") = "A"
   --    Parent_Directory ("A/B/C/..") = "A"
   --    Parent_Directory (Name) = Normalized_Compose (Name, "..")
   function Parent_Directory (
      Directory : String;
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String;

   --  extended
   --  There is a procedure version.
   procedure Parent_Directory (
      Directory : String;
      First : out Positive;
      Last : out Natural;
      Parent_Count : out Natural);

   --  exceptions

   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;

end Ada.Hierarchical_File_Names;
