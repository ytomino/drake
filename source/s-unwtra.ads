pragma License (Unrestricted);
pragma Compiler_Unit;
--  implementation package
package System.Unwind.Traceback is
   pragma Preelaborate;

   --  filled by gnatbind (init.c)
   Exception_Tracebacks : Integer := 0; --  set by "-E" option
   pragma Export (C, Exception_Tracebacks, "__gl_exception_tracebacks");

   --  get traceback (a-excach.adb)
   procedure Call_Chain (Current : not null Exception_Occurrence_Access);
   pragma Export (Ada, Call_Chain, "ada__exceptions__call_chain");
   pragma Weak_External (Call_Chain);

   --  equivalent to Append_Info_Exception_Information (a-exexda.adb)
   procedure Report_Traceback (Current : Exception_Occurrence);
   pragma Export (Ada, Report_Traceback, "ada__exceptions__report_traceback");
   pragma Weak_External (Report_Traceback);

end System.Unwind.Traceback;
