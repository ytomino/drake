pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.Interrupts;
package System.Native_Interrupts is

   subtype Interrupt_Id is Ada.Interrupts.Interrupt_Id;
   subtype Parameterless_Handler is Ada.Interrupts.Parameterless_Handler;

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean;

   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler;

   procedure Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id);

   procedure Raise_Interrupt (Interrupt : Interrupt_Id);

end System.Native_Interrupts;
