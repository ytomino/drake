pragma License (Unrestricted);
--  extended unit specialized for Windows
with Ada.IO_Exceptions;
package Ada.Hierarchical_File_Names is
   --  This package is "Pure" version of
   --    Ada.Directories.Hierarchical_File_Names.
   pragma Pure;

   --  path delimiter

   Default_Path_Delimiter : constant Character := '\';

   function Is_Path_Delimiter (Item : Character) return Boolean;
   pragma Inline (Is_Path_Delimiter);

   procedure Include_Trailing_Path_Delimiter (
      S : in out String;
      Last : in out Natural;
      Path_Delimiter : Character := Default_Path_Delimiter);
   procedure Exclude_Trailing_Path_Delimiter (
      S : String;
      Last : in out Natural);

   function Is_Drive_Letter (Item : Character) return Boolean;

   procedure Containing_Root_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural);

   --  operations in Ada.Directories

   function Simple_Name (Name : String) return String;

   --  modified
   function Containing_Directory (
      Name : String;
      Raise_On_Error : Boolean := True) -- additional
      return String;

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

   function Unfolded_Compose (
      Containing_Directory : String := "";
      Name : String;
      Extension : String := "")
      return String;
   function Unfolded_Compose (
      Containing_Directory : String := "";
      Name : String;
      Extension : String := "";
      Path_Delimiter : Character)
      return String;

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
      Extension : String := "")
      return String;
   function Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "";
      Path_Delimiter : Character)
      return String;

   --  extended
   --  This function returns the relative name from the base directory.
   --  For example: Relative_Name ("A", "B") = "../A",
   --    Relative_Name (Name, Initial_Directory (Name)) = Relative_Name (Name)
   function Relative_Name (
      Name : String;
      From : String)
      return String;
   function Relative_Name (
      Name : String;
      From : String;
      Path_Delimiter : Character)
      return String;

   --  exceptions

   Use_Error : exception
      renames IO_Exceptions.Use_Error;

   --  Note at A.16.1 (37/3).
   --  These subprograms does not raise Name_Error
   --    since no accessing any real external file.

end Ada.Hierarchical_File_Names;
