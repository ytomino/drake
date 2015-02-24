pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Unwind.Representation;
with System.Unwind.Searching;
with C.unwind;
separate (System.Unwind.Raising)
package body Separated is
   pragma Suppress (All_Checks);
   use type Representation.Machine_Occurrence_Access;
   use type C.signed_int;

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

   --  equivalent to Propagate_GCC_Exception (a-exexpr-gcc.adb)
   procedure Propagate_Machine_Occurrence (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access);
   pragma No_Return (Propagate_Machine_Occurrence);
   pragma Convention (C, Propagate_Machine_Occurrence);

   --  equivalent to Reraise_GCC_Exception (a-exexpr-gcc.adb)
   --  for nested controlled types
   procedure Reraise_Machine_Occurrence (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access);
   pragma Export (C, Reraise_Machine_Occurrence, "__gnat_reraise_zcx");

   --  Win64 SEH only (a-exexpr-gcc.adb)
   procedure Unhandled_Except_Handler (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access);
   pragma No_Return (Unhandled_Except_Handler);
   pragma Export (C, Unhandled_Except_Handler,
      "__gnat_unhandled_except_handler");

   --  for "catch exception" command of gdb (s-excdeb.ads)
   procedure Debug_Raise_Exception (E : not null Exception_Data_Access);
   pragma Export (Ada, Debug_Raise_Exception, "__gnat_debug_raise_exception");

   --  implementation

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
         Unhandled_Except_Handler (To_GNAT (Exception_Object));
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return C.unwind.URC_NO_REASON;
   end CleanupUnwind_Handler;

   --  (a-exexpr-gcc.adb)
   procedure Propagate_Exception (
      Machine_Occurrence :
         not null Representation.Machine_Occurrence_Access) is
   begin
      Set_Traceback (Machine_Occurrence.Occurrence);
      Debug_Raise_Exception (Machine_Occurrence.Occurrence.Id); -- for gdb
      Propagate_Machine_Occurrence (Machine_Occurrence);
   end Propagate_Exception;

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
      Unhandled_Except_Handler (Machine_Occurrence);
   end Propagate_Machine_Occurrence;

   procedure Reraise_Machine_Occurrence (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access)
      renames Propagate_Machine_Occurrence;

   procedure Unhandled_Except_Handler (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access)
   is
      Current : Exception_Occurrence_Access;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      Save_Current_Occurrence (Machine_Occurrence, Current);
      Unhandled_Exception_Terminate (Current);
   end Unhandled_Except_Handler;

   procedure Debug_Raise_Exception (E : not null Exception_Data_Access) is
      pragma Inspection_Point (E);
   begin
      null;
   end Debug_Raise_Exception;

end Separated;
