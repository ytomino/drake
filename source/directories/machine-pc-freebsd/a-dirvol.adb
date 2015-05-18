with Ada.Exception_Identification.From_Here;
with System.Native_Credentials;
with System.Zero_Terminated_Strings;
with C.errno;
package body Ada.Directories.Volumes is
   use Exception_Identification.From_Here;
   use type File_Size;
   use type C.signed_int;
   use type C.size_t;

   function Named_IO_Exception_Id (errno : C.signed_int)
      return Exception_Identification.Exception_Id
      renames System.Directory_Searching.Named_IO_Exception_Id;

   --  implementation

   function Where (Name : String) return File_System is
      C_Name : C.char_array (
         0 ..
         Name'Length * System.Zero_Terminated_Strings.Expanding);
   begin
      System.Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      return Result : File_System do
         if statfs (C_Name (0)'Access, Result'Unrestricted_Access) < 0 then
            Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
         end if;
      end return;
   end Where;

   function Size (FS : File_System) return File_Size is
   begin
      return File_Size (FS.f_blocks) * File_Size (FS.f_bsize);
   end Size;

   function Free_Space (FS : File_System) return File_Size is
   begin
      return File_Size (FS.f_bfree) * File_Size (FS.f_bsize);
   end Free_Space;

   function Owner (FS : File_System) return String is
   begin
      return System.Native_Credentials.User_Name (FS.f_owner);
   end Owner;

   function Format_Name (FS : File_System) return String is
   begin
      return System.Zero_Terminated_Strings.Value (FS.f_fstypename (0)'Access);
   end Format_Name;

   function Directory (FS : File_System) return String is
   begin
      return System.Zero_Terminated_Strings.Value (FS.f_mntonname (0)'Access);
   end Directory;

   function Device (FS : File_System) return String is
   begin
      return System.Zero_Terminated_Strings.Value (
         FS.f_mntfromname (0)'Access);
   end Device;

   function Case_Preserving (FS : File_System) return Boolean is
      pragma Unreferenced (FS);
   begin
      return True;
   end Case_Preserving;

   function Case_Sensitive (FS : File_System) return Boolean is
      pragma Unreferenced (FS);
   begin
      return True;
   end Case_Sensitive;

   function Is_HFS (FS : File_System) return Boolean is
      pragma Unreferenced (FS);
   begin
      return False;
   end Is_HFS;

end Ada.Directories.Volumes;
