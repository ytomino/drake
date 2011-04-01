pragma License (Unrestricted);
--  Ada 2012
package Ada.Directories.Hierarchical_File_Names is

   function Is_Simple_Name (Name : String) return Boolean;

   function Is_Root_Directory_Name (Name : String) return Boolean;

   function Is_Parent_Directory_Name (Name : String) return Boolean;

   function Is_Current_Directory_Name (Name : String) return Boolean;

   function Is_Full_Name (Name : String) return Boolean;

   function Is_Relative_Name (Name : String) return Boolean;

   function Simple_Name (Name : String) return String
      renames Directories.Simple_Name;

   function Containing_Directory (Name : String) return String
      renames Directories.Containing_Directory;

   function Initial_Directory (Name : String) return String;

   function Relative_Name (Name : String) return String;

   function Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "")
      return String;

   --  extended for extracting sub-filename
   procedure Initial_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural);
   procedure Relative_Name (
      Name : String;
      First : out Positive;
      Last : out Natural);

   --  extended for getting the relative name from the base directory
   --  for example: Relative_Name ("A", "B") = "../A"
   --  Relative_Name (Name, Initial_Directory (Name)) = Relative_Name (Name)
   function Relative_Name (Name : String; From : String)
      return String;

private

   --  for getting "../../.."
   function Parent_Directory_Name (Level : Positive) return String;

end Ada.Directories.Hierarchical_File_Names;
