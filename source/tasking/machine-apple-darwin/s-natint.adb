with C.signal;
package body System.Native_Interrupts is
   use type C.signed_int;

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean is
   begin
      return Interrupt not in
            1 .. C.signed_int'Max (C.signal.SIGRTMAX, C.signal.NSIG - 1)
         or else Interrupt = C.signal.SIGKILL
         or else Interrupt = C.signal.SIGSTOP;
   end Is_Reserved;

   procedure Raise_Interrupt (Interrupt : Interrupt_Id) is
   begin
      if C.signal.C_raise (Interrupt) < 0 then
         raise Program_Error;
      end if;
   end Raise_Interrupt;

end System.Native_Interrupts;
