pragma License (Unrestricted);
--  extended unit specialized for Linux
private with C.sys.statvfs;
package Ada.Directories.Volumes is
   --  File system information.

   type File_System is private;

   function Where (Name : String) return File_System;

   function Size (FS : File_System) return File_Size;
   function Free_Space (FS : File_System) return File_Size;

   function Case_Preserving (FS : File_System) return Boolean;
   function Case_Sensitive (FS : File_System) return Boolean;

   pragma Inline (Case_Preserving);
   pragma Inline (Case_Sensitive);

   --  unimplemented
   function Owner (FS : File_System) return String;
   function Format_Name (FS : File_System) return String;
   function Directory (FS : File_System) return String; -- mounted to
   function Device (FS : File_System) return String; -- mouted from
   pragma Import (Ada, Owner, "__drake_program_error");
   pragma Import (Ada, Format_Name, "__drake_program_error");
   pragma Import (Ada, Directory, "__drake_program_error");
   pragma Import (Ada, Device, "__drake_program_error");

private

   type File_System is record
      Info : aliased C.sys.statvfs.struct_statvfs64;
   end record;
   pragma Suppress_Initialization (File_System);

end Ada.Directories.Volumes;
