with Ada.Exceptions;
with C.copyfile;
with C.errno;
procedure Ada.Directories.Inside.Do_Copy_File (
   Source_Name : String;
   Target_Name : String;
   Form : String := "";
   Overwrite : Boolean := True)
is
   pragma Unreferenced (Form);
   use type C.signed_int;
   use type C.unsigned_int;
   Z_Source : String := Source_Name & Character'Val (0);
   C_Source : C.char_array (0 .. Z_Source'Length);
   for C_Source'Address use Z_Source'Address;
   Z_Target : String := Target_Name & Character'Val (0);
   C_Target : C.char_array (0 .. Z_Target'Length);
   for C_Target'Address use Z_Target'Address;
   Flag : C.unsigned_int := C.copyfile.COPYFILE_ALL;
begin
   if not Overwrite then
      Flag := Flag or C.copyfile.COPYFILE_EXCL;
   end if;
   if C.copyfile.copyfile (
      C_Source (0)'Access,
      C_Target (0)'Access,
      null,
      Flag) < 0
   then
      case C.errno.errno is
         when C.errno.ENOENT
            | C.errno.ENOTSUP =>
            Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
         when others =>
            Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end case;
   end if;
end Ada.Directories.Inside.Do_Copy_File;
