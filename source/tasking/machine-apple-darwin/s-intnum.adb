package body System.Interrupt_Numbers is

   function Is_Reserved (Interrupt : C.signed_int) return Boolean is
   begin
      return Interrupt not in First_Interrupt_Id .. Last_Interrupt_Id
         or else Interrupt = C.signal.SIGKILL
         or else Interrupt = C.signal.SIGSTOP;
   end Is_Reserved;

end System.Interrupt_Numbers;
