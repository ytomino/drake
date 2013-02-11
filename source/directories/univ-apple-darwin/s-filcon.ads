pragma License (Unrestricted);
--  implementation unit only in POSIX
with C.fcntl;
with C.sys.stat;
package System.File_Control is
   pragma Preelaborate;

   --  <fnctl.h>

   O_EXLOCK : constant := C.fcntl.O_EXLOCK;

   --  <sys/stat.h>

   subtype struct_stat is C.sys.stat.struct_stat;

   function lstat (
      path : access constant C.char;
      buf : access C.sys.stat.struct_stat)
      return C.signed_int
      renames C.sys.stat.lstat;

end System.File_Control;
