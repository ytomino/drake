with Ada.Exceptions;
with System.Zero_Terminated_Strings;
package body Ada.Directories.Volumes is
   use type C.signed_int;

   function Get_Where (Name : String) return File_System is
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      return Result : File_System do
         if statfs (C_Name (0)'Access, Result'Unrestricted_Access) < 0 then
            Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
         end if;
      end return;
   end Get_Where;

   function Get_Format_Name (FS : File_System) return String is
   begin
      return System.Zero_Terminated_Strings.Value (FS.f_fstypename'Address);
   end Get_Format_Name;

end Ada.Directories.Volumes;
