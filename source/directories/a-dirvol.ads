pragma License (Unrestricted);
--  extended unit
private with Ada.Finalization;
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
   pragma Inline (Where); -- renamed

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

   package Controlled is

      type File_System is limited private;

      function Reference (Object : Volumes.File_System)
         return not null
            access System.Native_Directories.Volumes.File_System;
      pragma Inline (Reference);

      function Where (Name : String) return Volumes.File_System;
         --  [gcc-7] strange error if this function is placed outside of
         --    the package Controlled, and Disable_Controlled => True

   private

      type File_System is
         limited new Finalization.Limited_Controlled with
      record
         Data : aliased System.Native_Directories.Volumes.File_System :=
            (others => <>);
      end record
         with
            Disable_Controlled =>
               System.Native_Directories.Volumes.Disable_Controlled;

      overriding procedure Finalize (Object : in out File_System);

   end Controlled;

   type File_System is new Controlled.File_System;

   function Where (Name : String) return File_System
      renames Controlled.Where;

   type File_System_Id is new System.Native_Directories.Volumes.File_System_Id;

end Ada.Directories.Volumes;
