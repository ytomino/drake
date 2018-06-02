--  for ZCX
pragma Check_Policy (Trace => Ignore);
with C.unwind;
separate (System.Unwind.Backtrace)
package body Separated is
   pragma Suppress (All_Checks);

   type Data is record
      Item : not null access Tracebacks_Array;
      Last : Natural;
      Exclude_Min : Address;
      Exclude_Max : Address;
   end record;
   pragma Suppress_Initialization (Data);

   function Unwind_Trace (
      Context : access C.unwind.struct_Unwind_Context;
      Argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code
      with Convention => C;

   function Unwind_Trace (
      Context : access C.unwind.struct_Unwind_Context;
      Argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code
   is
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      D : Data;
      for D'Address use Address (Argument);
      IP : constant Address :=
         System'To_Address (C.unwind.Unwind_GetIP (Context));
   begin
      if IP >= D.Exclude_Min and then IP <= D.Exclude_Max then
         D.Last := Tracebacks_Array'First - 1; -- reset
         pragma Check (Trace, Ada.Debug.Put ("exclude"));
      else
         D.Last := D.Last + 1;
         D.Item (D.Last) := IP;
         pragma Check (Trace, Ada.Debug.Put ("fill"));
         if D.Last >= Tracebacks_Array'Last then
            pragma Check (Trace, Ada.Debug.Put ("leave, over"));
            return C.unwind.URC_NORMAL_STOP;
         end if;
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return C.unwind.URC_NO_REASON;
   end Unwind_Trace;

   procedure Backtrace (
      Item : aliased out Tracebacks_Array;
      Last : out Natural;
      Exclude_Min : Address;
      Exclude_Max : Address)
   is
      D : aliased Data := (
         Item'Unchecked_Access,
         Tracebacks_Array'First - 1,
         Exclude_Min,
         Exclude_Max);
      Dummy : C.unwind.Unwind_Reason_Code;
   begin
      pragma Check (Trace, Ada.Debug.Put ("start"));
      Dummy := C.unwind.Unwind_Backtrace (
         Unwind_Trace'Access,
         C.void_ptr (D'Address));
      pragma Check (Trace, Ada.Debug.Put ("end"));
      Last := D.Last;
   end Backtrace;

end Separated;
