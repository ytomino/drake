pragma License (Unrestricted);
--  optional/overridable runtime unit
package System.Unwind.Backtrace is
   --  This package will be linked when "__gl_exception_tracebacks" is used.
   pragma Preelaborate;

   --  filled by gnatbind (init.c)
   Exception_Tracebacks : Integer := 0 -- set by "-E" option
      with Export,
         Convention => C, External_Name => "__gl_exception_tracebacks";

   --  backtrace (a-excach.adb)
   procedure Call_Chain (Current : in out Exception_Occurrence)
      with Export, -- for weak linking
         Convention => Ada, External_Name => "ada__exceptions__call_chain";

   pragma No_Inline (Call_Chain);

   --  equivalent to Append_Info_Basic_Exception_Traceback (a-exexda.adb)
   procedure Backtrace_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address))
      with Export, -- for weak linking
         Convention => Ada, External_Name => "__drake_backtrace_information";

   pragma No_Inline (Backtrace_Information);

   procedure Report_Backtrace (X : Exception_Occurrence)
      with Export, -- for weak linking
         Convention => Ada, External_Name => "__drake_report_backtrace";

   pragma No_Inline (Report_Backtrace);

end System.Unwind.Backtrace;
