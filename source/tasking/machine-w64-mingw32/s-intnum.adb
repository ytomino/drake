package body System.Interrupt_Numbers is

   function Is_Reserved (Interrupt : C.signed_int) return Boolean is
   begin
      return Interrupt not in First_Interrupt_Id .. Last_Interrupt_Id;
      --  SIGKILL and SIGSTOP are not declared in mingw
   end Is_Reserved;

end System.Interrupt_Numbers;
