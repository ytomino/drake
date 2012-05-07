pragma License (Unrestricted);
--  implementation unit
with C.sys.mount;
package Ada.Directories.Inside.File_Systems is

   type File_System is new C.sys.mount.struct_statfs;

   function Get_Where (Name : String) return File_System;
   function Get_Format_Name (FS : File_System) return String;

end Ada.Directories.Inside.File_Systems;
