pragma License (Unrestricted);
--  extended unit
private with System.File_Systems;
package Ada.Directories.Volumes is
   --  File system information.

   type File_System is limited private;

   function Where (Name : String) return File_System;

   function Size (FS : File_System) return File_Size;
   function Free_Space (FS : File_System) return File_Size;

   function Owner (FS : File_System) return String;
   function Format_Name (FS : File_System) return String;
   function Directory (FS : File_System) return String; -- mounted to
   function Device (FS : File_System) return String; -- mounted from

   function Case_Preserving (FS : File_System) return Boolean;
   function Case_Sensitive (FS : File_System) return Boolean;

   function Is_HFS (FS : File_System) return Boolean;

private

   type File_System is new System.File_Systems.File_System;

end Ada.Directories.Volumes;
