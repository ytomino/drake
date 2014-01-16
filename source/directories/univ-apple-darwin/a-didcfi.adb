with Ada.Exceptions;
with System.Zero_Terminated_Strings;
with C.copyfile;
with C.errno;
procedure Ada.Directories.Inside.Do_Copy_File (
   Source_Name : String;
   Target_Name : String;
   Overwrite : Boolean := True)
is
   use type C.signed_int;
   use type C.size_t;
   use type C.unsigned_int;
   C_Source_Name : C.char_array (
      0 ..
      Source_Name'Length * System.Zero_Terminated_Strings.Expanding);
   C_Target_Name : C.char_array (
      0 ..
      Target_Name'Length * System.Zero_Terminated_Strings.Expanding);
   Flag : C.unsigned_int := C.copyfile.COPYFILE_ALL;
begin
   System.Zero_Terminated_Strings.To_C (Source_Name, C_Source_Name (0)'Access);
   System.Zero_Terminated_Strings.To_C (Target_Name, C_Target_Name (0)'Access);
   if not Overwrite then
      Flag := Flag or C.copyfile.COPYFILE_EXCL;
   end if;
   if C.copyfile.copyfile (
      C_Source_Name (0)'Access,
      C_Target_Name (0)'Access,
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
