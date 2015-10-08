pragma License (Unrestricted);
--  extended unit
private with System.Native_Directories.Volumes;
package Ada.Directories.Volumes is
   --  File system information.

   type File_System is limited private;

--  subtype Assigned_File_System is File_System
--    with
--       Dynamic_Predicate => Is_Assigned (Assigned_File_System),
--       Predicate_Failure => raise Status_Error;

   function Is_Assigned (FS : File_System) return Boolean;

   function Where (Name : String) return File_System;

   function Size (
      FS : File_System) -- Assigned_File_System
      return File_Size;
   function Free_Space (
      FS : File_System) -- Assigned_File_System
      return File_Size;

   function Owner (
      FS : File_System) -- Assigned_File_System
      return String;
   function Format_Name (
      FS : File_System) -- Assigned_File_System
      return String;
   function Directory (
      FS : File_System) -- Assigned_File_System
      return String; -- mounted to
   function Device (
      FS : File_System) -- Assigned_File_System
      return String; -- mounted from

   function Case_Preserving (
      FS : File_System) -- Assigned_File_System
      return Boolean;
   function Case_Sensitive (
      FS : File_System) -- Assigned_File_System
      return Boolean;

   function Is_HFS (
      FS : File_System) -- Assigned_File_System
      return Boolean;

   type File_System_Id is private;

   function Identity (
      FS : File_System) -- Assigned_File_System
      return File_System_Id;

private

   type File_System is new System.Native_Directories.Volumes.File_System;

   type File_System_Id is new System.Native_Directories.Volumes.File_System_Id;

end Ada.Directories.Volumes;
