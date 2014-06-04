pragma License (Unrestricted);
--  runtime unit
with C.unwind;
package System.Unwind.Handling is
   pragma Preelaborate;

   function Unwind_RaiseException (
      exc : access C.unwind.struct_Unwind_Exception)
      return C.unwind.Unwind_Reason_Code
      renames C.unwind.Unwind_SjLj_RaiseException;

   function Unwind_ForcedUnwind (
      exc : access C.unwind.struct_Unwind_Exception;
      stop : C.unwind.Unwind_Stop_Fn;
      stop_argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code
      renames C.unwind.Unwind_SjLj_ForcedUnwind;

   --  (a-exexpr-gcc.adb)
   Others_Value : aliased constant C.unwind.Unwind_Ptr := 16#7FFF#;
   pragma Export (C, Others_Value, "__gnat_others_value");

   All_Others_Value : aliased constant C.unwind.Unwind_Ptr := 16#7FFF#;
   pragma Export (C, All_Others_Value, "__gnat_all_others_value");

   --  personality function (raise-gcc.c)
   function Personality (
      ABI_Version : C.signed_int;
      Phases : C.unwind.Unwind_Action;
      Exception_Class : C.unwind.Unwind_Exception_Class;
      Exception_Object : access C.unwind.struct_Unwind_Exception;
      Context : access C.unwind.struct_Unwind_Context)
      return C.unwind.Unwind_Reason_Code;
   pragma Export (C, Personality, "__gnat_personality_sj0");
   pragma Compile_Time_Error (
      Personality'Access = C.unwind.Unwind_Personality_Fn'(null),
      "this expression is always false, for type check purpose");

end System.Unwind.Handling;
