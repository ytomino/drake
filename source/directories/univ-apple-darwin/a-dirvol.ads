pragma License (Unrestricted);
--  extended unit
private with C.sys.mount;
package Ada.Directories.Volumes is
   --  File system information.

   type File_System is private;

   function Get_Where (Name : String) return File_System;
   function Get_Format_Name (FS : File_System) return String;

   function Is_HFS (FS : File_System) return Boolean;

private

   type File_System is new C.sys.mount.struct_statfs64;

end Ada.Directories.Volumes;
