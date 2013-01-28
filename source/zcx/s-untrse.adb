pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Debug;
with C.unwind;
separate (System.Unwind.Traceback)
package body Separated is
   pragma Suppress (All_Checks);

   type Data is record
      Traceback : not null access Tracebacks_Array;
      Length : not null access Natural;
      Exclude_Min : Address;
      Exclude_Max : Address;
      Skip_Frames : Natural;
   end record;
   pragma Suppress_Initialization (Data);

   function Unwind_Trace (
      Context : access C.unwind.struct_Unwind_Context;
      Argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code;
   pragma Convention (C, Unwind_Trace);

   function Unwind_Trace (
      Context : access C.unwind.struct_Unwind_Context;
      Argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code
   is
      function Cast is new Ada.Unchecked_Conversion (
         C.void_ptr,
         Address);
      D : Data;
      for D'Address use Cast (Argument);
   begin
      pragma Check (Trace, Debug.Put ("enter"));
      if D.Skip_Frames > 0 then
         D.Skip_Frames := D.Skip_Frames - 1;
         pragma Check (Trace, Debug.Put ("skip"));
         return C.unwind.URC_NO_REASON;
      elsif D.Length.all > D.Traceback'Last then
         pragma Check (Trace, Debug.Put ("over"));
         return C.unwind.URC_NORMAL_STOP;
      else
         declare
            function Cast is new Ada.Unchecked_Conversion (
               C.unwind.Unwind_Ptr,
               Address);
            IP : constant Address :=
               Cast (C.unwind.Unwind_GetIP (Context));
         begin
            if IP >= D.Exclude_Min and then IP <= D.Exclude_Max then
               pragma Check (Trace, Debug.Put ("exclude"));
               null;
            else
               D.Traceback (D.Traceback'First + D.Length.all) := IP;
               D.Length.all := D.Length.all + 1;
               pragma Check (Trace, Debug.Put ("fill"));
            end if;
         end;
         return C.unwind.URC_NO_REASON;
      end if;
   end Unwind_Trace;

   procedure Get_Traceback (
      Traceback : out Tracebacks_Array;
      Length : out Natural;
      Exclude_Min : Address;
      Exclude_Max : Address;
      Skip_Frames : Natural)
   is
      D : aliased Data := (
         Traceback'Unrestricted_Access,
         Length'Unrestricted_Access,
         Exclude_Min,
         Exclude_Max,
         Skip_Frames);
      Dummy : C.unwind.Unwind_Reason_Code;
      pragma Unreferenced (Dummy);
   begin
      Length := 0;
      Dummy := C.unwind.Unwind_Backtrace (
         Unwind_Trace'Access,
         C.void_ptr (D'Address));
   end Get_Traceback;

end Separated;
