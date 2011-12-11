pragma License (Unrestricted);
pragma Compiler_Unit;
--  runtime unit
package System.Unwind.Traceback is
   pragma Preelaborate;

   --  filled by gnatbind (init.c)
   Exception_Tracebacks : Integer := 0; -- set by "-E" option
   pragma Export (C, Exception_Tracebacks, "__gl_exception_tracebacks");

   --  get traceback (a-excach.adb)
   procedure Call_Chain (Current : not null Exception_Occurrence_Access);
   pragma Export (Ada, Call_Chain, "ada__exceptions__call_chain");

   --  equivalent to Append_Info_Exception_Information (a-exexda.adb)
   procedure Report_Traceback (Current : Exception_Occurrence);
   pragma Export (Ada, Report_Traceback, "ada__exceptions__report_traceback");

private

   --  for weak linking,
   --  these symbols will be linked when "__gl_exception_tracebacks" is used

   type Call_Chain_Handler is
      access procedure (Current : not null Exception_Occurrence_Access);
   Call_Chain_Ref : constant not null Call_Chain_Handler := Call_Chain'Access;
   pragma Export (Ada, Call_Chain_Ref, "__drake_ref_call_chain");

   type Report_Traceback_Handler is
      access procedure (Current : Exception_Occurrence);
   Report_Traceback_Ref : constant not null Report_Traceback_Handler :=
      Report_Traceback'Access;
   pragma Export (Ada, Report_Traceback_Ref, "__drake_ref_report_traceback");

end System.Unwind.Traceback;
