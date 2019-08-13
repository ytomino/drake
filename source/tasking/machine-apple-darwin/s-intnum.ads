pragma License (Unrestricted);
--  implementation unit specialized for Darwin (or FreeBSD)
with C.signal;
package System.Interrupt_Numbers is
   pragma Preelaborate;
   use type C.signed_int;

   First_Interrupt_Id : constant := C.signal.SIGHUP;
   Last_Interrupt_Id : constant :=
      Boolean'Pos (C.signal.SIGRTMAX > C.signal.NSIG - 1)
         * C.signal.SIGRTMAX
      + Boolean'Pos (C.signal.SIGRTMAX <= C.signal.NSIG - 1)
         * (C.signal.NSIG - 1);
      --  SIGUSR2 = NSIG - 1 = 31 in Darwin
      --  SIGRTMAX = 126 > NSIG - 1 = 31 in FreeBSD

   function Is_Reserved (Interrupt : C.signed_int) return Boolean;

end System.Interrupt_Numbers;
