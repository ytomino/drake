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

   function Containing_Directory (Name : String) return String
      renames Directories.Containing_Directory;

   function Initial_Directory (Name : String) return String
      renames Ada.Hierarchical_File_Names.Initial_Directory;

   function Relative_Name (Name : String) return String
      renames Ada.Hierarchical_File_Names.Relative_Name;

   --  modified
   function Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "";
      Path_Delimiter : Ada.Hierarchical_File_Names.Path_Delimiter_Type :=
         Ada.Hierarchical_File_Names.Default_Path_Delimiter) -- additional
      return String
      renames Ada.Hierarchical_File_Names.Compose;

end Ada.Directories.Hierarchical_File_Names;
