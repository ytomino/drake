pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C;
package System.Native_Interrupts is
   pragma Preelaborate;

   subtype Interrupt_Id is C.signed_int;

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean;

   procedure Raise_Interrupt (Interrupt : Interrupt_Id);

end System.Native_Interrupts;
