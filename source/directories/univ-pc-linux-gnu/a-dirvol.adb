with Ada.Exceptions;
with System.Zero_Terminated_Strings;
package body Ada.Directories.Volumes is
   use type File_Size;
   use type C.signed_int;
   use type C.size_t;

   function Where (Name : String) return File_System is
      C_Name : C.char_array (
         0 ..
         Name'Length * System.Zero_Terminated_Strings.Expanding);
   begin
      System.Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
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
