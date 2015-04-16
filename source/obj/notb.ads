pragma License (Unrestricted);
--  optional runtime unit
with System.Unwind;
package notb is
   pragma Preelaborate;

   Exception_Tracebacks : Integer
      with Export,
         Convention => C, External_Name => "__gl_exception_tracebacks";

   procedure Call_Chain (
      Current : in out System.Unwind.Exception_Occurrence) is null
      with Export, -- for weak linking
         Convention => Ada, External_Name => "ada__exceptions__call_chain";

   procedure Backtrace_Information (
      X : System.Unwind.Exception_Occurrence;
      Params : System.Address;
      Put : not null access procedure (S : String; Params : System.Address);
      New_Line : not null access procedure (Params : System.Address)) is null
      with Export, -- for weak linking
         Convention => Ada, External_Name => "__drake_backtrace_information";

   procedure Report_Backtrace (X : System.Unwind.Exception_Occurrence) is null
      with Export, -- for weak linking
         Convention => Ada, External_Name => "__drake_report_backtrace";

end notb;
