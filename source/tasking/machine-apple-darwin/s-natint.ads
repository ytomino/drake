pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with C;
package System.Native_Interrupts is
   pragma Preelaborate;

   subtype Interrupt_Id is C.signed_int;

   function Is_Blocked (Interrupt : Interrupt_Id) return Boolean;

   procedure Block (Interrupt : Interrupt_Id);
   procedure Unblock (Interrupt : Interrupt_Id);

   procedure Raise_Interrupt (Interrupt : Interrupt_Id);

end System.Native_Interrupts;
