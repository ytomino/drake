pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.Interrupts;
package System.Native_Interrupts.Vector is

   subtype Parameterless_Handler is Ada.Interrupts.Parameterless_Handler;

   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler;

   procedure Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id);

end System.Native_Interrupts.Vector;
