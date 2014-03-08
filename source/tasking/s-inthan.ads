pragma License (Unrestricted);
--  implementation unit
with Ada.Interrupts;
package System.Interrupt_Handlers is

   --  CXC3001 requires pragma Interrupt_Handler to Attach_Handler

   procedure Register_Interrupt_Handler (
      Code_Address : Address);
   procedure Register_Interrupt_Handler (
      Handler : Ada.Interrupts.Parameterless_Handler);

   procedure Set_Static_Handler (
      Code_Address : Address);
   procedure Set_Static_Handler (
      Handler : Ada.Interrupts.Parameterless_Handler);

   function Is_Static_Handler (
      Code_Address : Address)
      return Boolean;
   function Is_Static_Handler (
      Handler : Ada.Interrupts.Parameterless_Handler)
      return Boolean;

end System.Interrupt_Handlers;
