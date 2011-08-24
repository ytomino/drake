with C.copyfile;
with C.errno;
package body Ada.Directories.Inside is
   use type C.signed_int;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Form : String := "")
   is
      pragma Unreferenced (Form);
      Z_Source : String := Source_Name & Character'Val (0);
      C_Source : C.char_array (0 .. Z_Source'Length);
      for C_Source'Address use Z_Source'Address;
      Z_Target : String := Target_Name & Character'Val (0);
      C_Target : C.char_array (0 .. Z_Target'Length);
      for C_Target'Address use Z_Target'Address;
   begin
      if C.copyfile.copyfile (
         C_Source (0)'Access,
         C_Target (0)'Access,
         null,
         C.copyfile.COPYFILE_ALL) < 0
         --  add COPYFILE_EXCL to disable overwriting
      then
         case C.errno.errno is
            when C.errno.ENOENT
               | C.errno.ENOTSUP =>
               raise Name_Error;
            when others =>
               raise Use_Error;
         end case;
      end if;
   end Copy_File;

end Ada.Directories.Inside;
