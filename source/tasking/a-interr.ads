pragma License (Unrestricted);
--  with System;
--  with System.Multiprocessors;
package Ada.Interrupts is

   type Interrupt_Id is new Integer; -- implementation-defined
   type Parameterless_Handler is access protected procedure;

--  function Is_Reserved (Interrupt : Interrupt_Id) return Boolean;

--  function Is_Attached (Interrupt : Interrupt_Id) return Boolean;

--  function Current_Handler (Interrupt : Interrupt_Id)
--    return Parameterless_Handler;

--  procedure Attach_Handler (
--    New_Handler : Parameterless_Handler;
--    Interrupt : Interrupt_Id);

--  procedure Exchange_Handler (
--    Old_Handler : out Parameterless_Handler;
--    New_Handler : Parameterless_Handler;
--    Interrupt : Interrupt_Id);

--  procedure Detach_Handler (Interrupt : Interrupt_Id);

--  function Reference (Interrupt : Interrupt_Id) return System.Address;

--  function Get_CPU (Interrupt : Interrupt_Id)
--     return System.Multiprocessors.CPU_Range;

end Ada.Interrupts;
