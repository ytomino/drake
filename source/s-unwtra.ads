pragma License (Unrestricted);
--  runtime unit
package System.Unwind.Traceback is
   pragma Preelaborate;

   --  filled by gnatbind (init.c)
   Exception_Tracebacks : Integer := 0; -- set by "-E" option
   pragma Export (C, Exception_Tracebacks, "__gl_exception_tracebacks");

   --  get traceback (a-excach.adb)
   procedure Call_Chain (Current : not null Exception_Occurrence_Access);
   pragma Export (Ada, Call_Chain, "ada__exceptions__call_chain");

   --  equivalent to Append_Info_Basic_Exception_Traceback (a-exexda.adb)
   procedure Traceback_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address));

private

   --  for weak linking,
   --  these symbols will be linked when "__gl_exception_tracebacks" is used

   type Call_Chain_Handler is
      access procedure (Current : not null Exception_Occurrence_Access);
   Call_Chain_Ref : constant not null Call_Chain_Handler := Call_Chain'Access;
   pragma Export (Ada, Call_Chain_Ref, "__drake_ref_call_chain");

   type Traceback_Information_Handler is access procedure (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address));
   Traceback_Information_Ref : constant
      not null Traceback_Information_Handler :=
         Traceback_Information'Access;
   pragma Export (Ada, Traceback_Information_Ref,
      "__drake_ref_traceback_information");

end System.Unwind.Traceback;
