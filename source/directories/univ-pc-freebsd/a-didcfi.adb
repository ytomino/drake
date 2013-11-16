with Ada.Exceptions;
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
   use type C.signed_int;
   use type C.unsigned_int;
   use type C.signed_long; -- 64bit ssize_t
   Z_Source : String := Source_Name & Character'Val (0);
   C_Source : C.char_array (C.size_t);
   for C_Source'Address use Z_Source'Address;
   Source : C.signed_int;
   Z_Target : String := Target_Name & Character'Val (0);
   C_Target : C.char_array (C.size_t);
   for C_Target'Address use Z_Target'Address;
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
   Source := C.fcntl.open (
      C_Source (0)'Access,
      C.fcntl.O_RDONLY);
   if Source < 0 then
      Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
   end if;
   if C.sys.stat.fstat (Source, Data'Access) < 0 then
      Dummy := C.unistd.close (Source);
      Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
   end if;
   if not Overwrite then
      Flag := Flag or C.fcntl.O_EXCL;
   end if;
   Target := C.fcntl.open (
      C_Target (0)'Access,
      C.signed_int (Flag),
      Data.st_mode);
   if Target < 0 then
      Dummy := C.unistd.close (Source);
      Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
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
      Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
   end if;
end Ada.Directories.Inside.Do_Copy_File;
