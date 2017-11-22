pragma License (Unrestricted);
--  extended unit
private with System.Native_Directories.Temporary;
package Ada.Directories.Temporary is
   --  Creating temporary files and directories.

   --  settings

   function Temporary_Directory return String;
   procedure Set_Temporary_Directory (Name : String);

   pragma Inline (Temporary_Directory); -- renamed
   pragma Inline (Set_Temporary_Directory); -- renamed

   --  creating new entries

   function Create_Temporary_File (
      Directory : String := Temporary_Directory)
      return String;
   function Create_Temporary_Directory (
      Directory : String := Temporary_Directory)
      return String;

   pragma Inline (Create_Temporary_File); -- renamed
   pragma Inline (Create_Temporary_Directory); -- renamed

private

   function Temporary_Directory return String
      renames System.Native_Directories.Temporary.Temporary_Directory;

   procedure Set_Temporary_Directory (Name : String)
      renames System.Native_Directories.Temporary.Set_Temporary_Directory;

   function Create_Temporary_File (
      Directory : String := Temporary_Directory)
      return String
      renames System.Native_Directories.Temporary.Create_Temporary_File;

   function Create_Temporary_Directory (
      Directory : String := Temporary_Directory)
      return String
      renames System.Native_Directories.Temporary.Create_Temporary_Directory;

end Ada.Directories.Temporary;
