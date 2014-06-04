pragma Check_Policy (Trace, Off);
with Ada;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
package body System.Unwind.Representation is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   procedure memset (
      b : Address;
      c : Integer;
      n : Storage_Elements.Storage_Count);
   pragma Import (Intrinsic, memset, "__builtin_memset");

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

   --  implementation

   function New_Machine_Occurrence return not null Machine_Occurrence_Access is
      package Conv is
         new Address_To_Named_Access_Conversions (
            Machine_Occurrence,
            Machine_Occurrence_Access);
      Result : constant not null Machine_Occurrence_Access :=
         Conv.To_Pointer (
            Standard_Allocators.Allocate (
               Machine_Occurrence'Size / Standard'Storage_Unit));
   begin
      Result.Header.exception_class := GNAT_Exception_Class;
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

end System.Unwind.Representation;
