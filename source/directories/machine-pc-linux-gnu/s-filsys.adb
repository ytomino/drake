with Ada.Exception_Identification.From_Here;
with System.Native_Directories;
with System.Zero_Terminated_Strings;
with C.errno;
package body System.File_Systems is
   use Ada.Exception_Identification.From_Here;
   use type File_Size;
   use type C.signed_int;
   use type C.size_t;

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
      if C.sys.statvfs.statvfs64 (C_Name (0)'Access, FS.Info'Access) < 0 then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Get;

   function Size (FS : Non_Controlled_File_System) return File_Size is
   begin
      return File_Size (FS.Info.f_blocks) * File_Size (FS.Info.f_bsize);
   end Size;

   function Free_Space (FS : Non_Controlled_File_System) return File_Size is
   begin
      return File_Size (FS.Info.f_bfree) * File_Size (FS.Info.f_bsize);
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

   function Reference (Item : File_System)
      return not null access Non_Controlled_File_System is
   begin
      return Item.Data'Unrestricted_Access;
   end Reference;

end System.File_Systems;
