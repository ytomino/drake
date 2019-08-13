with System.Interrupt_Handlers;
with System.Interrupt_Numbers;
with System.Native_Interrupts.Vector;
package body Ada.Interrupts is

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean is
   begin
      return System.Interrupt_Numbers.Is_Reserved (
         System.Native_Interrupts.Interrupt_Id (Interrupt));
   end Is_Reserved;

   function Is_Blocked (Interrupt : Interrupt_Id) return Boolean is
   begin
      return System.Native_Interrupts.Is_Blocked (
         System.Native_Interrupts.Interrupt_Id (Interrupt));
   end Is_Blocked;

   procedure Block (Interrupt : Interrupt_Id) is
   begin
      System.Native_Interrupts.Block (
         System.Native_Interrupts.Interrupt_Id (Interrupt));
   end Block;

   procedure Unblock (Interrupt : Interrupt_Id) is
   begin
      System.Native_Interrupts.Unblock (
         System.Native_Interrupts.Interrupt_Id (Interrupt));
   end Unblock;

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
      return System.Native_Interrupts.Vector.Current_Handler (
         System.Native_Interrupts.Interrupt_Id (Interrupt));
   end Current_Handler;

   procedure Attach_Handler (
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id)
   is
      Dummy_Old_Handler : Parameterless_Handler;
   begin
      Exchange_Handler (Dummy_Old_Handler, New_Handler, Interrupt);
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
      Unchecked_Exchange_Handler (Old_Handler, New_Handler, Interrupt);
   end Exchange_Handler;

   procedure Detach_Handler (Interrupt : Interrupt_Id) is
      Dummy_Old_Handler : Parameterless_Handler;
   begin
      Exchange_Handler (Dummy_Old_Handler, null, Interrupt);
   end Detach_Handler;

   procedure Unchecked_Attach_Handler (
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id)
   is
      Dummy_Old_Handler : Parameterless_Handler;
   begin
      Unchecked_Exchange_Handler (Dummy_Old_Handler, New_Handler, Interrupt);
   end Unchecked_Attach_Handler;

   procedure Unchecked_Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id) is
   begin
      System.Native_Interrupts.Vector.Exchange_Handler (
         Old_Handler,
         New_Handler,
         System.Native_Interrupts.Interrupt_Id (Interrupt));
   end Unchecked_Exchange_Handler;

   procedure Unchecked_Detach_Handler (Interrupt : Interrupt_Id) is
      Dummy_Old_Handler : Parameterless_Handler;
   begin
      Unchecked_Exchange_Handler (Dummy_Old_Handler, null, Interrupt);
   end Unchecked_Detach_Handler;

   procedure Raise_Interrupt (Interrupt : Interrupt_Id) is
   begin
      System.Native_Interrupts.Raise_Interrupt (
         System.Native_Interrupts.Interrupt_Id (Interrupt));
   end Raise_Interrupt;

end Ada.Interrupts;
