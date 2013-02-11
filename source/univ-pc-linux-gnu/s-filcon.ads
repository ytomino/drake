pragma License (Unrestricted);
--  implementation unit only in POSIX
with C.sys.stat;
with C.sys.types;
with C.unistd;
package System.File_Control is
   pragma Preelaborate;

   --  <fnctl.h>

   O_SHLOCK : constant := 0;
   O_EXLOCK : constant := 0;

   --  <sys/stat.h>

   subtype struct_stat is C.sys.stat.struct_stat64;

   function fstat (
      fd : C.signed_int;
      buf : access C.sys.stat.struct_stat64)
      return C.signed_int
      renames C.sys.stat.fstat64;

   function lstat (
      path : access constant C.char;
      buf : access C.sys.stat.struct_stat64)
      return C.signed_int
      renames C.sys.stat.lstat64;

   --  <sys/types.h>

   subtype off_t is C.sys.types.off64_t;

   --  <unistd.h>

   function lseek (
      fd : C.signed_int;
      offset : C.sys.types.off64_t;
      whence : C.signed_int)
      return C.sys.types.off64_t
      renames C.unistd.lseek64;

end System.File_Control;
