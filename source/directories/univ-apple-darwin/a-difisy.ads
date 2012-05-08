pragma License (Unrestricted);
--  implementation unit
with C.sys.mount;
package Ada.Directories.Inside.File_Systems is

   type File_System is new C.sys.mount.struct_statfs64;

   function Get_Where (Name : String) return File_System;
   function Get_Format_Name (FS : File_System) return String;

   function Is_HFS (FS : File_System) return Boolean;

end Ada.Directories.Inside.File_Systems;
