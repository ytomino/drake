with Ada.Exception_Identification.From_Here;
with System.Zero_Terminated_Strings;
with C.copyfile;
with C.errno;
with C.stdio; -- rename(2)
with C.sys.stat;
package body Ada.Directories.Copying is
   use Exception_Identification.From_Here;
   use type C.signed_int;
   use type C.size_t;
   use type C.unsigned_int;

   function Named_IO_Exception_Id (errno : C.signed_int)
      return Exception_Identification.Exception_Id
      renames Directory_Searching.Named_IO_Exception_Id;

   --  implementation

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True)
   is
      C_Source_Name : C.char_array (
         0 ..
         Source_Name'Length * System.Zero_Terminated_Strings.Expanding);
      C_Target_Name : C.char_array (
         0 ..
         Target_Name'Length * System.Zero_Terminated_Strings.Expanding);
      Flag : C.unsigned_int :=
         C.copyfile.COPYFILE_ALL or C.copyfile.COPYFILE_NOFOLLOW;
   begin
      System.Zero_Terminated_Strings.To_C (
         Source_Name,
         C_Source_Name (0)'Access);
      System.Zero_Terminated_Strings.To_C (
         Target_Name,
         C_Target_Name (0)'Access);
      if not Overwrite then
         Flag := Flag or C.copyfile.COPYFILE_EXCL;
      end if;
      if C.copyfile.copyfile (
         C_Source_Name (0)'Access,
         C_Target_Name (0)'Access,
         null,
         Flag) < 0
      then
         declare
            errno : constant C.signed_int := C.errno.errno;
         begin
            case errno is
               when C.errno.ENOTSUP => -- the source is not a regular file
                  Raise_Exception (Name_Error'Identity);
               when others =>
                  Raise_Exception (Named_IO_Exception_Id (errno));
            end case;
         end;
      end if;
   end Copy_File;

   procedure Replace_File (
      Source_Name : String;
      Target_Name : String)
   is
      C_Source_Name : C.char_array (
         0 ..
         Source_Name'Length * System.Zero_Terminated_Strings.Expanding);
      C_Target_Name : C.char_array (
         0 ..
         Target_Name'Length * System.Zero_Terminated_Strings.Expanding);
      Info : aliased C.sys.stat.struct_stat;
      Error : Boolean;
   begin
      System.Zero_Terminated_Strings.To_C (
         Source_Name,
         C_Source_Name (0)'Access);
      System.Zero_Terminated_Strings.To_C (
         Target_Name,
         C_Target_Name (0)'Access);
      --  check whether the source is existing or not.
      Error := C.sys.stat.lstat (C_Source_Name (0)'Access, Info'Access) < 0;
      if not Error then
         --  copy attributes from the target to the source.
         Error := C.copyfile.copyfile (
            C_Target_Name (0)'Access,
            C_Source_Name (0)'Access,
            null,
            C.copyfile.COPYFILE_METADATA or C.copyfile.COPYFILE_NOFOLLOW) < 0;
         if not Error
            or else C.errno.errno = C.errno.ENOENT -- target is not existing
         then
            --  overwrite the target with the source.
            Error := C.stdio.rename (
               C_Source_Name (0)'Access,
               C_Target_Name (0)'Access) < 0;
         end if;
      end if;
      if Error then
         declare
            errno : constant C.signed_int := C.errno.errno;
         begin
            case errno is
               when C.errno.ENOTSUP => -- the source is not a regular file
                  Raise_Exception (Name_Error'Identity);
               when others =>
                  Raise_Exception (Named_IO_Exception_Id (errno));
            end case;
         end;
      end if;
   end Replace_File;

end Ada.Directories.Copying;
