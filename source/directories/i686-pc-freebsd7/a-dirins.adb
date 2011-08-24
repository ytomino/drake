with System;
with C.unistd;
with C.sys.fcntl;
with C.sys.mman;
with C.sys.types;
package body Ada.Directories.Inside is
   use type C.signed_int;
   use type C.unsigned_int;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Form : String := "")
   is
      pragma Unreferenced (Form);
      Z_Source : String := Source_Name & Character'Val (0);
      C_Source : C.char_array (0 .. Z_Source'Length);
      for C_Source'Address use Z_Source'Address;
      Source : C.signed_int;
      Z_Target : String := Target_Name & Character'Val (0);
      C_Target : C.char_array (0 .. Z_Target'Length);
      for C_Target'Address use Z_Target'Address;
      Target : C.signed_int;
      Data : aliased C.sys.stat.struct_stat;
      Map : C.void_ptr;
      Written : C.sys.types.ssize_t;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Source := C.sys.fcntl.open (
         C_Source (0)'Access,
         C.sys.fcntl.O_RDONLY);
      if Source < 0 then
         raise Name_Error;
      end if;
      if C.sys.stat.fstat (Source, Data'Access) < 0 then
         Dummy := C.unistd.close (Source);
         raise Use_Error;
      end if;
      Target := C.sys.fcntl.open (
         C_Target (0)'Access,
         C.signed_int (C.unsigned_int'(
            C.sys.fcntl.O_WRONLY or
            C.sys.fcntl.O_CREAT or
            C.sys.fcntl.O_EXLOCK)),
            --  add O_EXCL to disable overwriting
         Data.st_mode);
      if Target < 0 then
         Dummy := C.unistd.close (Source);
         raise Name_Error;
      end if;
      Map := C.sys.mman.mmap (
         C.void_ptr (System.Null_Address),
         C.size_t (Data.st_size),
         C.sys.mman.PROT_READ,
         C.sys.mman.MAP_FILE + C.sys.mman.MAP_SHARED,
         Source,
         0);
      Written := C.unistd.write (
         Target,
         C.void_const_ptr (Map),
         C.size_t (Data.st_size));
      Dummy := C.sys.mman.munmap (Map, C.size_t (Data.st_size));
      Dummy := C.unistd.close (Source);
      Dummy := C.unistd.close (Target);
      if Written < C.sys.types.ssize_t (Data.st_size) then
         raise Use_Error;
      end if;
   end Copy_File;

end Ada.Directories.Inside;
