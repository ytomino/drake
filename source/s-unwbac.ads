pragma License (Unrestricted);
--  runtime unit
package System.Unwind.Backtrace is
   pragma Preelaborate;

   --  filled by gnatbind (init.c)
   Exception_Tracebacks : Integer := 0; -- set by "-E" option
   pragma Export (C, Exception_Tracebacks, "__gl_exception_tracebacks");

   --  backtrace (a-excach.adb)
   procedure Call_Chain (Current : in out Exception_Occurrence);
   pragma Export (Ada, Call_Chain, "ada__exceptions__call_chain");

   --  equivalent to Append_Info_Basic_Exception_Traceback (a-exexda.adb)
   procedure Backtrace_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address));

   procedure Report_Backtrace (X : Exception_Occurrence);

private

   --  for weak linking,
   --  these symbols will be linked when "__gl_exception_tracebacks" is used

   type Call_Chain_Handler is
      access procedure (Current : in out Exception_Occurrence);
   Call_Chain_Ref : constant not null Call_Chain_Handler := Call_Chain'Access;
   pragma Export (Ada, Call_Chain_Ref, "__drake_ref_call_chain");

   type Backtrace_Information_Handler is access procedure (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address));
   Backtrace_Information_Ref : constant
      not null Backtrace_Information_Handler :=
      Backtrace_Information'Access;
   pragma Export (Ada, Backtrace_Information_Ref,
      "__drake_ref_backtrace_information");

   type Report_Backtrace_Handler is
      access procedure (X : Exception_Occurrence);
   Report_Backtrace_Ref : constant not null Report_Backtrace_Handler :=
      Report_Backtrace'Access;
   pragma Export (Ada, Report_Backtrace_Ref, "__drake_ref_report_backtrace");

end System.Unwind.Backtrace;
