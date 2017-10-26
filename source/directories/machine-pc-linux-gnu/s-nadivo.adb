with Ada.Exception_Identification.From_Here;
with System.Zero_Terminated_Strings;
with C.errno;
package body System.Native_Directories.Volumes is
   use Ada.Exception_Identification.From_Here;
   use type File_Size;
   use type C.signed_int;
   use type C.signed_long; -- f_type in 64bit
   use type C.size_t;

   --  implementation

   function Is_Assigned (FS : File_System) return Boolean is
   begin
      return FS.Statistics.f_type /= 0;
   end Is_Assigned;

   procedure Get (Name : String; FS : aliased out File_System) is
      C_Name : C.char_array (
         0 ..
         Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      if C.sys.statfs.statfs (C_Name (0)'Access, FS.Statistics'Access) < 0 then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Get;

   function Size (FS : File_System) return File_Size is
   begin
      return File_Size (FS.Statistics.f_blocks)
         * File_Size (FS.Statistics.f_bsize);
   end Size;

   function Free_Space (FS : File_System) return File_Size is
   begin
      return File_Size (FS.Statistics.f_bfree)
         * File_Size (FS.Statistics.f_bsize);
   end Free_Space;

   function Identity (FS : File_System) return File_System_Id is
   begin
      return FS.Statistics.f_fsid;
   end Identity;

end System.Native_Directories.Volumes;
