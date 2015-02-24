pragma License (Unrestricted);
--  runtime unit
with System.Unwind.Representation;
package System.Unwind.Handling is
   pragma Preelaborate;

   --  hook for entering an exception handler (a-exexpr-gcc.adb)
   procedure Begin_Handler (
      Machine_Occurrence : Representation.Machine_Occurrence_Access);
   pragma Export (C, Begin_Handler, "__gnat_begin_handler");

   --  hook for leaving an exception handler (a-exexpr-gcc.adb)
   procedure End_Handler (
      Machine_Occurrence : Representation.Machine_Occurrence_Access);
   pragma Export (C, End_Handler, "__gnat_end_handler");

end System.Unwind.Handling;
