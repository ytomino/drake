pragma License (Unrestricted);
--  implementation unit only in POSIX
with C.fcntl;
with C.sys.stat;
with C.sys.types;
with C.unistd;
package System.File_Control is
   pragma Preelaborate;

   --  <fnctl.h>

   O_SHLOCK : constant := C.fcntl.O_SHLOCK;
   O_EXLOCK : constant := C.fcntl.O_EXLOCK;

   --  <sys/stat.h>

   subtype struct_stat is C.sys.stat.struct_stat;

   function fstat (
      fildes : C.signed_int;
      buf : access C.sys.stat.struct_stat)
      return C.signed_int
      renames C.sys.stat.fstat;

   function lstat (
      path : access constant C.char;
      buf : access C.sys.stat.struct_stat)
      return C.signed_int
      renames C.sys.stat.lstat;

   --  <sys/types.h>

   subtype off_t is C.sys.types.off_t;

   --  <unistd.h>

   function lseek (
      fildes : C.signed_int;
      offset : C.sys.types.off_t;
      whence : C.signed_int)
      return C.sys.types.off_t
      renames C.unistd.lseek;

end System.File_Control;
