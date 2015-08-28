with System.Interrupt_Handlers;
with System.Native_Interrupts;
package body Ada.Interrupts is

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean
      renames System.Native_Interrupts.Is_Reserved;

   function Is_Attached (Interrupt : Interrupt_Id) return Boolean is
   begin
      return Current_Handler (Interrupt) /= null;
   end Is_Attached;

   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;
      return System.Native_Interrupts.Current_Handler (Interrupt);
   end Current_Handler;

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
      Interrupt : Interrupt_Id) is
   begin
      Old_Handler := Current_Handler (Interrupt);
      if (Old_Handler /= null
         and then System.Interrupt_Handlers.Is_Static_Handler (Old_Handler))
         or else (
            New_Handler /= null
            and then System.Interrupt_Handlers.Is_Static_Handler (New_Handler))
      then
         raise Program_Error;
      end if;
      System.Native_Interrupts.Exchange_Handler (
         Old_Handler,
         New_Handler,
         Interrupt);
   end Exchange_Handler;

   procedure Detach_Handler (Interrupt : Interrupt_Id) is
      Old_Handler : Parameterless_Handler;
      pragma Unreferenced (Old_Handler);
   begin
      Exchange_Handler (Old_Handler, null, Interrupt);
   end Detach_Handler;

   procedure Unchecked_Attach_Handler (
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id)
   is
      Old_Handler : Parameterless_Handler;
      pragma Unreferenced (Old_Handler);
   begin
      Unchecked_Exchange_Handler (Old_Handler, New_Handler, Interrupt);
   end Unchecked_Attach_Handler;

   procedure Unchecked_Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id)
      renames System.Native_Interrupts.Exchange_Handler;

   procedure Raise_Interrupt (Interrupt : Interrupt_Id)
      renames System.Native_Interrupts.Raise_Interrupt;

end Ada.Interrupts;
