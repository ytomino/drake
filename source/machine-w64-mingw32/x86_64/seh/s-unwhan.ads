pragma License (Unrestricted);
--  runtime unit
with C.excpt;
with C.unwind;
with C.winnt;
package System.Unwind.Handling is
   pragma Preelaborate;

   function Unwind_RaiseException (
      exc : access C.unwind.struct_Unwind_Exception)
      return C.unwind.Unwind_Reason_Code
      renames C.unwind.Unwind_RaiseException;

   function Unwind_ForcedUnwind (
      exc : access C.unwind.struct_Unwind_Exception;
      stop : C.unwind.Unwind_Stop_Fn;
      stop_argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code
      renames C.unwind.Unwind_ForcedUnwind;

   --  (a-exexpr-gcc.adb)
   Others_Value : aliased constant C.char := 'O';
   pragma Export (C, Others_Value, "__gnat_others_value");

   All_Others_Value : aliased constant C.char := 'A';
   pragma Export (C, All_Others_Value, "__gnat_all_others_value");

   Unhandled_Others_Value : aliased constant C.char := 'U'; -- SEH only
   pragma Export (C, Unhandled_Others_Value, "__gnat_unhandled_others_value");

   --  personality function (raise-gcc.c)
   function Personality (
      ABI_Version : C.signed_int;
      Phases : C.unwind.Unwind_Action;
      Exception_Class : C.unwind.Unwind_Exception_Class;
      Exception_Object : access C.unwind.struct_Unwind_Exception;
      Context : access C.unwind.struct_Unwind_Context)
      return C.unwind.Unwind_Reason_Code;
   pragma Export (C, Personality, "__gnat_personality_imp");
   pragma Compile_Time_Error (
      Personality'Access = C.unwind.Unwind_Personality_Fn'(null),
      "this expression is always false, for type check purpose");

   --  personality function (raise-gcc.c)
   function Personality_SEH (
      ms_exc : C.winnt.PEXCEPTION_RECORD;
      this_frame : C.void_ptr;
      ms_orig_context : C.winnt.PCONTEXT;
      ms_disp : C.winnt.PDISPATCHER_CONTEXT)
      return C.excpt.EXCEPTION_DISPOSITION;
   pragma Export (C, Personality_SEH, "__gnat_personality_seh0");

end System.Unwind.Handling;
