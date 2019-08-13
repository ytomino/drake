package body System.Interrupt_Numbers is
   use type C.signed_int;

   function Is_Reserved (Interrupt : C.signed_int) return Boolean is
   begin
      return Interrupt not in First_Interrupt_Id .. Last_Interrupt_Id
         or else Interrupt = C.signal.SIGKILL
         or else Interrupt = C.signal.SIGSTOP
         or else Interrupt in 32 .. C.signal.SIGRTMIN - 1; -- reserved by glibc
   end Is_Reserved;

end System.Interrupt_Numbers;
