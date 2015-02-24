pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Unwind.Mapping;
with System.Unwind.Representation;
with System.Unwind.Searching;
with C.unwind;
separate (System.Unwind.Raising)
package body Separated is
   pragma Suppress (All_Checks);
   use type Representation.Machine_Occurrence_Access;
   use type C.signed_int;
   use type C.unsigned_int; -- Unwind_Ptr (32bit)
   use type C.unsigned_long; -- Unwind_Ptr and Unwind_Exception_Class (64bit)
   use type C.unsigned_long_long; -- Unwind_Exception_Class (32bit)

   Foreign_Exception : aliased Exception_Data;
   pragma Import (Ada, Foreign_Exception,
      "system__exceptions__foreign_exception");

   function To_GNAT is
      new Ada.Unchecked_Conversion (
         C.unwind.struct_Unwind_Exception_ptr,
         Representation.Machine_Occurrence_Access);

   --  (a-exexpr-gcc.adb)
   procedure Set_Foreign_Occurrence (
      X : out Exception_Occurrence;
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access);

   --  equivalent to Setup_Current_Excep (a-exexpr-gcc.adb)
   procedure Save_Current_Occurrence (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access;
      Current : out Exception_Occurrence_Access);

   --  hook for entering an exception handler (a-exexpr-gcc.adb)
   procedure Begin_Handler (
      Machine_Occurrence : Representation.Machine_Occurrence_Access);
   pragma Export (C, Begin_Handler, "__gnat_begin_handler");

   --  hook for leaving an exception handler (a-exexpr-gcc.adb)
   procedure End_Handler (
      Machine_Occurrence : Representation.Machine_Occurrence_Access);
   pragma Export (C, End_Handler, "__gnat_end_handler");

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

   --  when E : others (a-exexpr-gcc.adb)
   procedure Set_Exception_Parameter (
      X : not null Exception_Occurrence_Access;
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access);
   pragma Export (C, Set_Exception_Parameter,
      "__gnat_set_exception_parameter");

   --  for "catch exception" command of gdb (s-excdeb.ads)
   procedure Debug_Raise_Exception (E : not null Exception_Data_Access);
   pragma Export (Ada, Debug_Raise_Exception, "__gnat_debug_raise_exception");

   --  implementation

   procedure Set_Foreign_Occurrence (
      X : out Exception_Occurrence;
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access)
   is
      package MOA_Conv is
         new Address_To_Named_Access_Conversions (
            Representation.Machine_Occurrence,
            Representation.Machine_Occurrence_Access);
   begin
      X.Id := Foreign_Exception'Access;
      X.Machine_Occurrence := MOA_Conv.To_Address (Machine_Occurrence);
      X.Msg_Length := 0;
      X.Exception_Raised := True;
      X.Pid := Local_Partition_ID;
      X.Num_Tracebacks := 0;
   end Set_Foreign_Occurrence;

   procedure Save_Current_Occurrence (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access;
      Current : out Exception_Occurrence_Access)
   is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
   begin
      Current := TLS.Current_Exception'Access;
      if Machine_Occurrence.Header.exception_class =
         Representation.GNAT_Exception_Class
      then
         Current.all := Machine_Occurrence.Occurrence;
      else
         Set_Foreign_Occurrence (Current.all, Machine_Occurrence);
      end if;
   end Save_Current_Occurrence;

   procedure Begin_Handler (
      Machine_Occurrence : Representation.Machine_Occurrence_Access)
   is
      Current : Exception_Occurrence_Access;
      pragma Unreferenced (Current);
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      Save_Current_Occurrence (Machine_Occurrence, Current);
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Begin_Handler;

   procedure End_Handler (
      Machine_Occurrence : Representation.Machine_Occurrence_Access) is
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      if Machine_Occurrence = null then
         pragma Check (Trace, Ada.Debug.Put (
            "Machine_Occurrence = null, reraised"));
         null;
      else
         C.unwind.Unwind_DeleteException (Machine_Occurrence.Header'Access);
         --  in Win32 SEH, the chain may be rollback, so restore it
         Mapping.Reinstall_Exception_Handler;
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end End_Handler;

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
      X : Exception_Occurrence;
      Stack_Guard : Address)
   is
      Machine_Occurrence : Representation.Machine_Occurrence_Access;
   begin
      Machine_Occurrence := Representation.New_Machine_Occurrence;
      Machine_Occurrence.Occurrence := X;
      Machine_Occurrence.Stack_Guard := Stack_Guard;
      if Call_Chain'Address /= Null_Address then
         Call_Chain (Machine_Occurrence.Occurrence'Access);
         declare
            function Report return Boolean;
            function Report return Boolean is
            begin
               Report_Traceback (Machine_Occurrence.Occurrence);
               return True;
            end Report;
         begin
            pragma Check (Trace, Ada.Debug.Put ("raising..."));
            pragma Check (Trace, Report);
         end;
      end if;
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

   procedure Set_Exception_Parameter (
      X : not null Exception_Occurrence_Access;
      Machine_Occurrence :
         not null Representation.Machine_Occurrence_Access) is
   begin
      if Machine_Occurrence.Header.exception_class =
         Representation.GNAT_Exception_Class
      then
         Save_Occurrence (X.all, Machine_Occurrence.Occurrence);
      else
         Set_Foreign_Occurrence (X.all, Machine_Occurrence);
      end if;
   end Set_Exception_Parameter;

   procedure Debug_Raise_Exception (E : not null Exception_Data_Access) is
      pragma Inspection_Point (E);
   begin
      null;
   end Debug_Raise_Exception;

end Separated;
