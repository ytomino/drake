pragma License (Unrestricted);
--  implementation unit only in POSIX
with C.fcntl;
package System.File_Control is
   pragma Preelaborate;

   --  <fnctl.h>

   O_SHLOCK : constant := C.fcntl.O_SHLOCK;
   O_EXLOCK : constant := C.fcntl.O_EXLOCK;

end System.File_Control;
