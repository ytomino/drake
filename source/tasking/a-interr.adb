with Ada.Interrupts.Inside;
package body Ada.Interrupts is

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean
      renames Inside.Is_Reserved;

   function Is_Attached (Interrupt : Interrupt_Id) return Boolean is
   begin
      return Current_Handler (Interrupt) /= null;
   end Is_Attached;

   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler
      renames Inside.Current_Handler;

   procedure Attach_Handler (
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id)
   is
      Old_Handler : Parameterless_Handler;
      pragma Unreferenced (Old_Handler);
   begin
      Exchange_Handler (Old_Handler, New_Handler, Interrupt);
   end Attach_Handler;

   procedure Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id)
      renames Inside.Exchange_Handler;

   procedure Detach_Handler (Interrupt : Interrupt_Id) is
      Old_Handler : Parameterless_Handler;
      pragma Unreferenced (Old_Handler);
   begin
      Exchange_Handler (Old_Handler, null, Interrupt);
   end Detach_Handler;

   procedure Raise_Interrupt (Interrupt : Interrupt_Id)
      renames Inside.Raise_Interrupt;

end Ada.Interrupts;
