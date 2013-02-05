with Ada.Exceptions;
with System.Zero_Terminated_Strings;
with C.stdint;
package body Ada.Directories.Volumes is
   use type C.signed_int;
   use type C.stdint.uint32_t;

   function Get_Where (Name : String) return File_System is
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      return Result : File_System do
         if statfs64 (C_Name (0)'Access, Result'Unrestricted_Access) < 0 then
            Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
         end if;
      end return;
   end Get_Where;

   function Get_Format_Name (FS : File_System) return String is
   begin
      return System.Zero_Terminated_Strings.Value (FS.f_fstypename'Address);
   end Get_Format_Name;

   function Is_HFS (FS : File_System) return Boolean is
   begin
      return FS.f_type = 17; -- VT_HFS
   end Is_HFS;

end Ada.Directories.Volumes;
