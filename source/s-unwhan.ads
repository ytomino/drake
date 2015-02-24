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

   --  when E : others (a-exexpr-gcc.adb)
   procedure Set_Exception_Parameter (
      X : not null Exception_Occurrence_Access;
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access);
   pragma Export (C, Set_Exception_Parameter,
      "__gnat_set_exception_parameter");

end System.Unwind.Handling;
