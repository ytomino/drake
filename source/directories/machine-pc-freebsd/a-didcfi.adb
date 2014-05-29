with Ada.Exception_Identification.From_Here;
with System.Zero_Terminated_Strings;
with C.fcntl;
with C.sys.mman;
with C.sys.stat;
with C.sys.types;
with C.unistd;
procedure Ada.Directories.Inside.Do_Copy_File (
   Source_Name : String;
   Target_Name : String;
   Overwrite : Boolean := True)
is
   use Exception_Identification.From_Here;
   use type C.signed_int;
   use type C.unsigned_int;
   use type C.signed_long; -- 64bit ssize_t
   use type C.size_t;
   C_Source_Name : C.char_array (
      0 ..
      Source_Name'Length * System.Zero_Terminated_Strings.Expanding);
   Source : C.signed_int;
   C_Target_Name : C.char_array (
      0 ..
      Target_Name'Length * System.Zero_Terminated_Strings.Expanding);
   Target : C.signed_int;
   Flag : C.unsigned_int :=
      C.fcntl.O_WRONLY or
      C.fcntl.O_CREAT or
      C.fcntl.O_EXLOCK;
   Data : aliased C.sys.stat.struct_stat;
   Map : C.void_ptr;
   Written : C.sys.types.ssize_t;
   Dummy : C.signed_int;
   pragma Unreferenced (Dummy);
begin
   System.Zero_Terminated_Strings.To_C (Source_Name, C_Source_Name (0)'Access);
   System.Zero_Terminated_Strings.To_C (Target_Name, C_Target_Name (0)'Access);
   Source := C.fcntl.open (
      C_Source_Name (0)'Access,
      C.fcntl.O_RDONLY);
   if Source < 0 then
      Raise_Exception (Name_Error'Identity);
   end if;
   if C.sys.stat.fstat (Source, Data'Access) < 0 then
      Dummy := C.unistd.close (Source);
      Raise_Exception (Use_Error'Identity);
   end if;
   if not Overwrite then
      Flag := Flag or C.fcntl.O_EXCL;
   end if;
   Target := C.fcntl.open (
      C_Target_Name (0)'Access,
      C.signed_int (Flag),
      Data.st_mode);
   if Target < 0 then
      Dummy := C.unistd.close (Source);
      Raise_Exception (Name_Error'Identity);
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
      Raise_Exception (Use_Error'Identity);
   end if;
end Ada.Directories.Inside.Do_Copy_File;
