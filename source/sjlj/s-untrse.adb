pragma Check_Policy (Trace, Off);
with Ada;
with System.Address_To_Constant_Access_Conversions;
separate (System.Unwind.Traceback)
package body Separated is
   pragma Suppress (All_Checks);
   --  in sjlj mode, Unwind_Backtrace does not work

   procedure main;
   pragma Import (C, main);

   type Address_Access is access constant Address;
   for Address_Access'Storage_Size use 0;

   function builtin_frame_address (A1 : Natural) return Address_Access;
   pragma Import (Intrinsic, builtin_frame_address, "__builtin_frame_address");

   Parent_Offset : constant := 0; -- [BP + x] = Parent's BP
   Return_Offset : constant := Standard'Address_Size / Standard'Storage_Unit;

   Call_Length : constant := 0; -- follow Unwind_Backtrace
   --  1 + Standard'Address_Size / Standard'Storage_Unit;
   --  length of call instruction

   Caller_In_main : constant := 160;
   --  120 in unoptimized 32bit code

   procedure Get_Traceback (
      Traceback : out Tracebacks_Array;
      Length : out Natural;
      Exclude_Min : Address;
      Exclude_Max : Address;
      Skip_Frames : Natural)
   is
      package Conv is
         new Address_To_Constant_Access_Conversions (Address, Address_Access);
      BP : Address;
      IP : Address;
      Skip : Natural;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      BP := Conv.To_Address (builtin_frame_address (0));
      Length := 0;
      Skip := Skip_Frames;
      loop
         if Traceback'First + Length > Traceback'Last then
            pragma Check (Trace, Ada.Debug.Put ("over"));
            exit;
         end if;
         IP := Conv.To_Pointer (BP + Return_Offset).all - Call_Length;
         if Skip > 0 then
            Skip := Skip - 1;
            pragma Check (Trace, Ada.Debug.Put ("skip"));
         elsif IP >= Exclude_Min and then IP <= Exclude_Max then
            pragma Check (Trace, Ada.Debug.Put ("exclude"));
            null;
         else
            Traceback (Traceback'First + Length) := IP;
            Length := Length + 1;
            pragma Check (Trace, Ada.Debug.Put ("fill"));
         end if;
         exit when IP >= main'Code_Address
            and then IP < main'Code_Address + Caller_In_main;
         BP := Conv.To_Pointer (BP).all + Parent_Offset;
         exit when BP = 0; -- dirty handling in thread
      end loop;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Get_Traceback;

end Separated;
