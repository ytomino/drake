with Ada.Exception_Identification.From_Here;
with System.Native_Credentials;
with System.Native_Directories;
with System.Zero_Terminated_Strings;
with C.errno;
with C.stdint;
with C.unistd;
package body System.File_Systems is
   use Ada.Exception_Identification.From_Here;
   use type File_Size;
   use type C.signed_int;
   use type C.signed_long;
   use type C.size_t;
   use type C.stdint.uint32_t;

   function IO_Exception_Id (errno : C.signed_int)
      return Ada.Exception_Identification.Exception_Id
      renames Native_Directories.IO_Exception_Id;

   function Named_IO_Exception_Id (errno : C.signed_int)
      return Ada.Exception_Identification.Exception_Id
      renames Native_Directories.Named_IO_Exception_Id;

   --  implementation

   procedure Get (
      Name : String;
      FS : aliased out Non_Controlled_File_System)
   is
      C_Name : C.char_array (
         0 ..
         Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      FS.Case_Sensitive_Valid := False;
      if C.sys.mount.statfs64 (
         C_Name (0)'Access,
         FS.Statistics'Access) < 0
      then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Get;

   function Size (FS : Non_Controlled_File_System) return File_Size is
   begin
      return File_Size (FS.Statistics.f_blocks)
         * File_Size (FS.Statistics.f_bsize);
   end Size;

   function Free_Space (FS : Non_Controlled_File_System) return File_Size is
   begin
      return File_Size (FS.Statistics.f_bfree)
         * File_Size (FS.Statistics.f_bsize);
   end Free_Space;

   function Owner (FS : Non_Controlled_File_System) return String is
   begin
      return Native_Credentials.User_Name (FS.Statistics.f_owner);
   end Owner;

   function Format_Name (FS : Non_Controlled_File_System) return String is
   begin
      return Zero_Terminated_Strings.Value (
         FS.Statistics.f_fstypename (0)'Access);
   end Format_Name;

   function Directory (FS : Non_Controlled_File_System) return String is
   begin
      return Zero_Terminated_Strings.Value (
         FS.Statistics.f_mntonname (0)'Access);
   end Directory;

   function Device (FS : Non_Controlled_File_System) return String is
   begin
      return Zero_Terminated_Strings.Value (
         FS.Statistics.f_mntfromname (0)'Access);
   end Device;

   function Case_Preserving (FS : Non_Controlled_File_System) return Boolean is
      R : C.signed_long;
   begin
      R := C.unistd.pathconf (
         FS.Statistics.f_mntonname (0)'Access,
         C.unistd.PC_CASE_PRESERVING);
      if R < 0 then
         Raise_Exception (IO_Exception_Id (C.errno.errno));
      end if;
      return R /= 0;
   end Case_Preserving;

   function Case_Sensitive (FS : aliased in out Non_Controlled_File_System)
      return Boolean is
   begin
      if not FS.Case_Sensitive_Valid then
         declare
            R : C.signed_long;
         begin
            R := C.unistd.pathconf (
               FS.Statistics.f_mntonname (0)'Access,
               C.unistd.PC_CASE_SENSITIVE);
            if R < 0 then
               Raise_Exception (IO_Exception_Id (C.errno.errno));
            end if;
            FS.Case_Sensitive := R /= 0;
            FS.Case_Sensitive_Valid := True;
         end;
      end if;
      return FS.Case_Sensitive;
   end Case_Sensitive;

   function Is_HFS (FS : Non_Controlled_File_System) return Boolean is
   begin
      return FS.Statistics.f_type = 17; -- VT_HFS
   end Is_HFS;

   function Reference (Item : File_System)
      return not null access Non_Controlled_File_System is
   begin
      return Item.Data'Unrestricted_Access;
   end Reference;

end System.File_Systems;
