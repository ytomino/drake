pragma License (Unrestricted);
--  Ada 2012
package Ada.Directories.Hierarchical_File_Names is

   function Is_Simple_Name (Name : String) return Boolean
      renames Ada.Hierarchical_File_Names.Is_Simple_Name;

   function Is_Root_Directory_Name (Name : String) return Boolean
      renames Ada.Hierarchical_File_Names.Is_Root_Directory_Name;

   function Is_Parent_Directory_Name (Name : String) return Boolean
      renames Ada.Hierarchical_File_Names.Is_Parent_Directory_Name;

   function Is_Current_Directory_Name (Name : String) return Boolean
      renames Ada.Hierarchical_File_Names.Is_Current_Directory_Name;

   function Is_Full_Name (Name : String) return Boolean
      renames Ada.Hierarchical_File_Names.Is_Full_Name;

   function Is_Relative_Name (Name : String) return Boolean
      renames Ada.Hierarchical_File_Names.Is_Relative_Name;

   function Simple_Name (Name : String) return String
      renames Directories.Simple_Name;

   --  modified
   function Containing_Directory (
      Name : String;
      Raise_On_Error : Boolean := True) -- additional
      return String
      renames Directories.Containing_Directory;

   function Initial_Directory (Name : String) return String
      renames Ada.Hierarchical_File_Names.Initial_Directory;

   function Relative_Name (Name : String) return String
      renames Ada.Hierarchical_File_Names.Relative_Name;

   function Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "")
      return String
      renames Ada.Hierarchical_File_Names.Compose;

   --  extended
   --  There are procedure version.
   procedure Initial_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural)
      renames Ada.Hierarchical_File_Names.Initial_Directory;
   procedure Relative_Name (
      Name : String;
      First : out Positive;
      Last : out Natural)
      renames Ada.Hierarchical_File_Names.Relative_Name;

   --  extended
   --  This function returns the relative name from the base directory.
   --  For example: Relative_Name ("A", "B") = "../A",
   --    Relative_Name (Name, Initial_Directory (Name)) = Relative_Name (Name)
   function Relative_Name (Name : String; From : String)
      return String
      renames Ada.Hierarchical_File_Names.Relative_Name;

end Ada.Directories.Hierarchical_File_Names;
