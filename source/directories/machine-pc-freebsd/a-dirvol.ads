pragma License (Unrestricted);
--  extended unit specialized for FreeBSD
private with C.sys.mount;
package Ada.Directories.Volumes is
   --  File system information.

   type File_System is private;

   function Where (Name : String) return File_System;

   function Size (FS : File_System) return File_Size;
   function Free_Space (FS : File_System) return File_Size;

   function Owner (FS : File_System) return String;
   function Format_Name (FS : File_System) return String;
   function Directory (FS : File_System) return String; -- mounted to
   function Device (FS : File_System) return String; -- mouted from

private

   type File_System is new C.sys.mount.struct_statfs;

end Ada.Directories.Volumes;
