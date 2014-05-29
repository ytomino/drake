with C.winbase;
with C.winnt;
package body System.Native_Time is
   pragma Suppress (All_Checks);

   Diff : constant Nanosecond_Number := 173247552000000000;
   --  100-nanosecond from 1601-01-01 (0 of FILETIME)
   --    to 2150-01-01 (0 of Ada time)

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
      U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 255, -- any value in others
         QuadPart => C.winnt.ULONGLONG (
            Nanosecond_Number'Integer_Value (T) / 100 + Diff));
   begin
      return (
         dwLowDateTime => U.LowPart,
         dwHighDateTime => U.HighPart);
   end To_Native_Time;

   function To_Time (T : Native_Time) return Duration is
      U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 0,
         LowPart => T.dwLowDateTime,
         HighPart => T.dwHighDateTime);
   begin
      return Duration'Fixed_Value (
         (Nanosecond_Number (U.QuadPart) - Diff) * 100);
   end To_Time;

   function Clock return Native_Time is
      Result : aliased C.windef.FILETIME;
   begin
      C.winbase.GetSystemTimeAsFileTime (Result'Access);
      return Result;
   end Clock;

   procedure Simple_Delay_For (D : Duration) is
   begin
      C.winbase.Sleep (C.windef.DWORD (D * 1000.0));
   end Simple_Delay_For;

   procedure Delay_For (D : Duration) is
   begin
      if D >= 0.0 then
         Delay_For_Hook.all (D);
      end if;
   end Delay_For;

   procedure Simple_Delay_Until (T : Native_Time) is
   begin
      Simple_Delay_For (To_Time (T) - To_Time (Clock));
   end Simple_Delay_Until;

   procedure Delay_Until (T : Native_Time) is
   begin
      Delay_Until_Hook.all (T);
   end Delay_Until;

   procedure Generic_Delay_Until (T : Ada_Time) is
   begin
      Delay_Until (To_Native_Time (Duration (T)));
   end Generic_Delay_Until;

end System.Native_Time;
