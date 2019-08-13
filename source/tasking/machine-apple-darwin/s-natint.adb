with C.signal;
package body System.Native_Interrupts is
   use type C.signed_int;

   procedure Mask (How : C.signed_int; Interrupt : Interrupt_Id);
   procedure Mask (How : C.signed_int; Interrupt : Interrupt_Id) is
      Mask : aliased C.signal.sigset_t;
      Dummy : C.signed_int;
      R : C.signed_int;
   begin
      Dummy := C.signal.sigemptyset (Mask'Access);
      Dummy := C.signal.sigaddset (Mask'Access, Interrupt);
      R := C.signal.sigprocmask (How, Mask'Access, null);
      if R < 0 then
         raise Program_Error;
      end if;
   end Mask;

   --  implementation

   function Is_Blocked (Interrupt : Interrupt_Id) return Boolean is
      Current_Mask : aliased C.signal.sigset_t;
      R : C.signed_int;
   begin
      R := C.signal.sigprocmask (
         C.signal.SIG_SETMASK,
         null,
         Current_Mask'Access);
      if R < 0 then
         raise Program_Error;
      end if;
      return C.signal.sigismember (Current_Mask'Access, Interrupt) /= 0;
   end Is_Blocked;

   procedure Block (Interrupt : Interrupt_Id) is
   begin
      Mask (C.signal.SIG_BLOCK, Interrupt);
   end Block;

   procedure Unblock (Interrupt : Interrupt_Id) is
   begin
      Mask (C.signal.SIG_UNBLOCK, Interrupt);
   end Unblock;

   procedure Raise_Interrupt (Interrupt : Interrupt_Id) is
   begin
      if C.signal.C_raise (Interrupt) < 0 then
         raise Program_Error;
      end if;
   end Raise_Interrupt;

end System.Native_Interrupts;
