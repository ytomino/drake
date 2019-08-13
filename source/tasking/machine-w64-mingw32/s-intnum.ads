pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C.signal;
package System.Interrupt_Numbers is
   pragma Preelaborate;

   First_Interrupt_Id : constant := 1;
   Last_Interrupt_Id : constant := C.signal.NSIG - 1;

   function Is_Reserved (Interrupt : C.signed_int) return Boolean;

end System.Interrupt_Numbers;
