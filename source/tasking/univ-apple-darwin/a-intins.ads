pragma License (Unrestricted);
--  implementation unit
package Ada.Interrupts.Inside is

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean;

   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler;

   procedure Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id);

   procedure Raise_Interrupt (Interrupt : Interrupt_Id);

end Ada.Interrupts.Inside;
