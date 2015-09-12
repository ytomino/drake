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

   procedure Get (
      Name : String;
      FS : aliased out Non_Controlled_File_System)
   is
      C_Name : C.char_array (
         0 ..
         Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      if C.sys.statfs.statfs (C_Name (0)'Access, FS'Access) < 0 then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Get;

   function Size (FS : Non_Controlled_File_System) return File_Size is
   begin
      return File_Size (FS.f_blocks) * File_Size (FS.f_bsize);
   end Size;

   function Free_Space (FS : Non_Controlled_File_System) return File_Size is
   begin
      return File_Size (FS.f_bfree) * File_Size (FS.f_bsize);
   end Free_Space;

   function Case_Preserving (FS : Non_Controlled_File_System) return Boolean is
      pragma Unreferenced (FS);
   begin
      return True;
   end Case_Preserving;

   function Case_Sensitive (FS : Non_Controlled_File_System) return Boolean is
      pragma Unreferenced (FS);
   begin
      return True;
   end Case_Sensitive;

   function Is_HFS (FS : Non_Controlled_File_System) return Boolean is
      pragma Unreferenced (FS);
   begin
      return False;
   end Is_HFS;

   function Identity (FS : Non_Controlled_File_System) return File_System_Id is
   begin
      return FS.f_fsid;
   end Identity;

   function Reference (Item : File_System)
      return not null access Non_Controlled_File_System is
   begin
      return Item.Data'Unrestricted_Access;
   end Reference;

end System.Native_Directories.Volumes;
