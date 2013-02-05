with Ada.Exceptions;
package body Ada.Directories.Volumes is
   use type File_Size;
   use type C.signed_int;

   function Where (Name : String) return File_System is
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      return Result : File_System do
         if C.sys.statvfs.statvfs64 (
            C_Name (0)'Access,
            Result.Info'Access) < 0
         then
            Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
         end if;
      end return;
   end Where;

   function Size (FS : File_System) return File_Size is
   begin
      return File_Size (FS.Info.f_blocks) * File_Size (FS.Info.f_bsize);
   end Size;

   function Free_Space (FS : File_System) return File_Size is
   begin
      return File_Size (FS.Info.f_bfree) * File_Size (FS.Info.f_bsize);
   end Free_Space;

end Ada.Directories.Volumes;
