pragma License (Unrestricted);
--  implementation unit specialized for Darwin (or FreeBSD)
with C.fcntl;
package System.File_Control is
   pragma Preelaborate;

   --  <fnctl.h>

   O_SHLOCK : constant := C.fcntl.O_SHLOCK;
   O_EXLOCK : constant := C.fcntl.O_EXLOCK;

   O_CLOEXEC : constant := 0;

end System.File_Control;
