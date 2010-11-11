pragma License (Unrestricted);
--  with System;
package Ada.Interrupts is

   type Interrupt_ID is new Integer; --  implementation-defined
   type Parameterless_Handler is access protected procedure;

--  function Is_Reserved (Interrupt : Interrupt_ID) return Boolean;

--  function Is_Attached (Interrupt : Interrupt_ID) return Boolean;

--  function Current_Handler (Interrupt : Interrupt_ID)
--    return Parameterless_Handler;

--  procedure Attach_Handler (
--    New_Handler : Parameterless_Handler;
--    Interrupt : Interrupt_ID);

--  procedure Exchange_Handler (
--    Old_Handler : out Parameterless_Handler;
--    New_Handler : Parameterless_Handler;
--    Interrupt : Interrupt_ID);

--  procedure Detach_Handler (Interrupt : Interrupt_ID);

--  function Reference (Interrupt : Interrupt_ID) return System.Address;

end Ada.Interrupts;
