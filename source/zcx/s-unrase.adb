pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Unwind.Representation;
with System.Unwind.Searching;
with C.unwind;
separate (System.Unwind.Raising)
package body Separated is
   pragma Suppress (All_Checks);
   use type C.unwind.Unwind_Action;

   function To_GNAT is
      new Ada.Unchecked_Conversion (
         C.unwind.struct_Unwind_Exception_ptr,
         Representation.Machine_Occurrence_Access);

   --  (a-exexpr-gcc.adb)
   function CleanupUnwind_Handler (
      ABI_Version : C.signed_int;
      Phases : C.unwind.Unwind_Action;
      Exception_Class : C.unwind.Unwind_Exception_Class;
      Exception_Object : access C.unwind.struct_Unwind_Exception;
      Context : access C.unwind.struct_Unwind_Context;
      Argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code;
   pragma Convention (C, CleanupUnwind_Handler);

   function CleanupUnwind_Handler (
      ABI_Version : C.signed_int;
      Phases : C.unwind.Unwind_Action;
      Exception_Class : C.unwind.Unwind_Exception_Class;
      Exception_Object : access C.unwind.struct_Unwind_Exception;
      Context : access C.unwind.struct_Unwind_Context;
      Argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code
   is
      pragma Unreferenced (ABI_Version);
      pragma Unreferenced (Exception_Class);
      pragma Unreferenced (Context);
      pragma Unreferenced (Argument);
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      if Phases >= C.unwind.UA_END_OF_STACK then
         Occurrences.Unhandled_Except_Handler (To_GNAT (Exception_Object));
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return C.unwind.URC_NO_REASON;
   end CleanupUnwind_Handler;

   --  implementation

   procedure Propagate_Machine_Occurrence (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access)
   is
      Dummy : C.unwind.Unwind_Reason_Code;
      pragma Unreferenced (Dummy);
   begin
      pragma Check (Trace, Ada.Debug.Put ("Unwind_RaiseException"));
      Dummy := Searching.Unwind_RaiseException (
         Machine_Occurrence.Header'Access);
      --  in GNAT runtime, calling Notify_Unhandled_Exception here
      pragma Check (Trace, Ada.Debug.Put ("Unwind_ForcedUnwind"));
      Dummy := Searching.Unwind_ForcedUnwind (
         Machine_Occurrence.Header'Access,
         CleanupUnwind_Handler'Access,
         C.void_ptr (Null_Address));
      pragma Check (Trace, Ada.Debug.Put ("unhandled"));
      Occurrences.Unhandled_Except_Handler (Machine_Occurrence);
   end Propagate_Machine_Occurrence;

end Separated;
