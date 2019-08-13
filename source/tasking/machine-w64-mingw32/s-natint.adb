with C.signal;
package body System.Native_Interrupts is
   use type C.signed_int;

   procedure Raise_Interrupt (Interrupt : Interrupt_Id) is
   begin
      if C.signal.C_raise (C.signed_int (Interrupt)) < 0 then
         raise Program_Error;
      end if;
   end Raise_Interrupt;

end System.Native_Interrupts;
