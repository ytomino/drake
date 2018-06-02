pragma License (Unrestricted);
--  with System;
--  with System.Multiprocessors;
package Ada.Interrupts is

   type Interrupt_Id is range 0 .. 2 ** 16 - 1; -- implementation-defined
   type Parameterless_Handler is access protected procedure;

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean;
   pragma Inline (Is_Reserved);

   --  extended
   --  Check the interrupt mask of current process.
   function Is_Blocked (Interrupt : Interrupt_Id) return Boolean;
   pragma Inline (Is_Blocked);

   --  extended
   --  Set the interrupt mask of current process.
   procedure Block (Interrupt : Interrupt_Id);
   --  extended
   --  Unset the interrupt mask of current process.
   procedure Unblock (Interrupt : Interrupt_Id);

   pragma Inline (Block);
   pragma Inline (Unblock);

   function Is_Attached (Interrupt : Interrupt_Id) return Boolean;
   pragma Inline (Is_Attached);

   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler;

   procedure Attach_Handler (
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id);
   pragma Inline (Attach_Handler);

   procedure Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id);

   procedure Detach_Handler (Interrupt : Interrupt_Id);
   pragma Inline (Detach_Handler);

   --  extended
   --  Unchecked version of Attach_Handler.
   procedure Unchecked_Attach_Handler (
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id);
   pragma Inline (Unchecked_Attach_Handler);

   --  extended
   --  Unchecked version of Exchange_Handler.
   procedure Unchecked_Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id);
   pragma Inline (Unchecked_Exchange_Handler);

   --  extended
   --  Unchecked version of Detach_Handler.
   procedure Unchecked_Detach_Handler (Interrupt : Interrupt_Id);
   pragma Inline (Unchecked_Detach_Handler);

--  function Reference (Interrupt : Interrupt_Id) return System.Address;

--  function Get_CPU (Interrupt : Interrupt_Id)
--     return System.Multiprocessors.CPU_Range;

   --  extended
   --  Raise a interrupt from/to itself.
   procedure Raise_Interrupt (Interrupt : Interrupt_Id);
   pragma Inline (Raise_Interrupt);

end Ada.Interrupts;
