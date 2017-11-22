pragma License (Unrestricted);
--  implementation unit
with Ada.Interrupts;
package System.Interrupt_Handlers is

   --  pragma Interrupt_Handler is required to Attach_Handler, CXC3001.

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
