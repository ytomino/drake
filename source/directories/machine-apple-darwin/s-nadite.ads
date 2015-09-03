pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
package System.Native_Directories.Temporary is
   --  There are subprograms to create temporary file or directory.
   pragma Preelaborate;

   function Temporary_Directory return String;
   procedure Set_Temporary_Directory (Name : String);

   function Create_Temporary_File (Directory : String) return String;
   function Create_Temporary_Directory (Directory : String) return String;

end System.Native_Directories.Temporary;
