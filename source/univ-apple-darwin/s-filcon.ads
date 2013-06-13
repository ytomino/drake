pragma License (Unrestricted);
--  implementation unit specialized for Darwin
with C.fcntl;
package System.File_Control is
   pragma Preelaborate;

   --  <fnctl.h>

   O_SHLOCK : constant := C.fcntl.O_SHLOCK;
   O_EXLOCK : constant := C.fcntl.O_EXLOCK;

   O_CLOEXEC : constant := C.fcntl.O_CLOEXEC;

end System.File_Control;
