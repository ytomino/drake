pragma License (Unrestricted);
--  implementation unit only in POSIX
with C.sys.stat;
package System.File_Control is
   pragma Preelaborate;

   --  <fnctl.h>

   O_EXLOCK : constant := 0;

   --  <sys/stat.h>

   subtype struct_stat is C.sys.stat.struct_stat64;

   function lstat (
      path : access constant C.char;
      buf : access C.sys.stat.struct_stat64)
      return C.signed_int
      renames C.sys.stat.lstat64;

end System.File_Control;
