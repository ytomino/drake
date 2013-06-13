pragma License (Unrestricted);
--  implementation unit specialized for Linux
with C.fcntl;
package System.File_Control is
   pragma Preelaborate;

   --  <fnctl.h>

   O_SHLOCK : constant := 0;
   O_EXLOCK : constant := 0;

   O_CLOEXEC : constant := C.fcntl.O_CLOEXEC;

end System.File_Control;
