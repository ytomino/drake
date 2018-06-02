--  for SjLj
pragma Check_Policy (Trace => Ignore);
with System.Address_To_Constant_Access_Conversions;
separate (System.Unwind.Backtrace)
package body Separated is
   pragma Suppress (All_Checks);

   --  Note: In SjLj mode, Unwind_Backtrace does not work.

   procedure main
      with Import, Convention => C;

   type Address_Access is access constant Address;
   for Address_Access'Storage_Size use 0;

   package AA_Conv is
      new Address_To_Constant_Access_Conversions (Address, Address_Access);

   function builtin_frame_address (A1 : Natural) return Address_Access
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_frame_address";

   Parent_Offset : constant := 0; -- [BP + x] = Parent's BP
   Return_Offset : constant := Standard'Address_Size / Standard'Storage_Unit;

   Call_Length : constant := 0; -- follow Unwind_Backtrace
--    1 + Standard'Address_Size / Standard'Storage_Unit; -- call instruction

   Caller_In_main : constant := 160; -- >= 120 in unoptimized 32bit code

   procedure Backtrace (
      Item : aliased out Tracebacks_Array;
      Last : out Natural;
      Exclude_Min : Address;
      Exclude_Max : Address)
   is
      BP : Address;
      IP : Address;
   begin
      pragma Check (Trace, Ada.Debug.Put ("start"));
      BP := AA_Conv.To_Address (builtin_frame_address (0));
      Last := Tracebacks_Array'First - 1;
      loop
         IP := AA_Conv.To_Pointer (BP + Return_Offset).all - Call_Length;
         if IP >= Exclude_Min and then IP <= Exclude_Max then
            Last := Tracebacks_Array'First - 1; -- reset
            pragma Check (Trace, Ada.Debug.Put ("exclude"));
         else
            Last := Last + 1;
            Item (Last) := IP;
            pragma Check (Trace, Ada.Debug.Put ("fill"));
            exit when Last >= Tracebacks_Array'Last;
         end if;
         exit when IP >= main'Code_Address
            and then IP < main'Code_Address + Caller_In_main;
         BP := AA_Conv.To_Pointer (BP).all + Parent_Offset;
         exit when BP = 0; -- dirty handling in thread
      end loop;
      pragma Check (Trace, Ada.Debug.Put ("end"));
   end Backtrace;

end Separated;
