pragma License (Unrestricted);
--  implementation unit specialized for Linux
with C.signal;
package System.Interrupt_Numbers is
   pragma Preelaborate;

   First_Interrupt_Id : constant := C.signal.SIGHUP;
   Last_Interrupt_Id : constant := C.signal.NSIG - 1;
      --  SIGRTMAX (__libc_current_sigrtmax) = NSIG - 1 = 64

   function Is_Reserved (Interrupt : C.signed_int) return Boolean;

end System.Interrupt_Numbers;
