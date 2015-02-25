pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Standard_Allocators;
with System.Unwind.Representation;
with System.Unwind.Searching;
with C.unwind;
separate (System.Unwind.Raising)
package body Separated is
   pragma Suppress (All_Checks);
   use type Representation.Machine_Occurrence_Access;
   use type Storage_Elements.Storage_Offset;
   use type C.signed_int;

   procedure memset (
      b : Address;
      c : Integer;
      n : Storage_Elements.Storage_Count);
   pragma Import (Intrinsic, memset, "__builtin_memset");

   function To_GNAT is
      new Ada.Unchecked_Conversion (
         C.unwind.struct_Unwind_Exception_ptr,
         Representation.Machine_Occurrence_Access);

   --  equivalent to GNAT_GCC_Exception_Cleanup (a-exexpr-gcc.adb)
   procedure Cleanup (
      Reason : C.unwind.Unwind_Reason_Code;
      Exception_Object : access C.unwind.struct_Unwind_Exception);
   pragma Convention (C, Cleanup);

   procedure Cleanup (
      Reason : C.unwind.Unwind_Reason_Code;
      Exception_Object : access C.unwind.struct_Unwind_Exception)
   is
      pragma Unreferenced (Reason);
      package Conv is
         new Address_To_Named_Access_Conversions (
            C.unwind.struct_Unwind_Exception,
            C.unwind.struct_Unwind_Exception_ptr);
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      Standard_Allocators.Free (Conv.To_Address (Exception_Object));
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Cleanup;

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
         Unhandled_Except_Handler (To_GNAT (Exception_Object));
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return C.unwind.URC_NO_REASON;
   end CleanupUnwind_Handler;

   --  implementation

   function New_Machine_Occurrence
      return not null Representation.Machine_Occurrence_Access
   is
      package Conv is
         new Address_To_Named_Access_Conversions (
            Representation.Machine_Occurrence,
            Representation.Machine_Occurrence_Access);
      Result : constant not null Representation.Machine_Occurrence_Access :=
         Conv.To_Pointer (
            Standard_Allocators.Allocate (
               Representation.Machine_Occurrence'Size
               / Standard'Storage_Unit));
   begin
      Result.Header.exception_class := Representation.GNAT_Exception_Class;
      Result.Header.exception_cleanup := Cleanup'Access;
      --  fill 0 to private area
      pragma Compile_Time_Error (
         C.unwind.Unwind_Exception_Class'Size rem Standard'Word_Size /= 0,
         "unaligned Unwind_Exception_Class'Size");
      pragma Compile_Time_Error (
         C.unwind.Unwind_Exception_Cleanup_Fn'Size rem Standard'Word_Size /= 0,
         "unaligned Unwind_Exception_Cleanup_Fn'Size");
      memset (
         Result.Header.exception_cleanup'Address
            + Storage_Elements.Storage_Offset'(
               C.unwind.Unwind_Exception_Cleanup_Fn'Size
               / Standard'Storage_Unit),
         0,
         C.unwind.struct_Unwind_Exception'Size / Standard'Storage_Unit
            - Storage_Elements.Storage_Offset'(
               C.unwind.Unwind_Exception_Class'Size
                  / Standard'Storage_Unit
               + C.unwind.Unwind_Exception_Cleanup_Fn'Size
                  / Standard'Storage_Unit));
      return Result;
   end New_Machine_Occurrence;

   procedure Free (
      Machine_Occurrence : Representation.Machine_Occurrence_Access) is
   begin
      C.unwind.Unwind_DeleteException (Machine_Occurrence.Header'Access);
   end Free;

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

end Separated;
