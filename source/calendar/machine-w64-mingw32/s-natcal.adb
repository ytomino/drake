with System.Native_Time;
with C.winbase;
with C.winnt;
package body System.Native_Calendar is
   use type System.Native_Time.Nanosecond_Number;
   use type C.windef.WINBOOL;

   Diff : constant := 17324755200_000_000_0;
      --  100-nanoseconds from 1601-01-01 (0 of FILETIME)
      --    to 2150-01-01 (0 of Ada time)

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
      U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 255, -- any value in others
         QuadPart => C.winnt.ULONGLONG (
            System.Native_Time.Nanosecond_Number'Integer_Value (T) / 100
            + Diff));
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
         (System.Native_Time.Nanosecond_Number (U.QuadPart) - Diff) * 100);
   end To_Time;

   function Clock return Native_Time is
      Result : aliased C.windef.FILETIME;
   begin
      C.winbase.GetSystemTimeAsFileTime (Result'Access);
      return Result;
   end Clock;

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Hour : out Hour_Number;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration;
      Leap_Second : out Boolean;
      Day_of_Week : out Day_Name;
      Time_Zone : Time_Offset;
      Error : out Boolean)
   is
      Local_Date : Duration;
      FileTime : aliased C.windef.FILETIME;
      SystemTime : aliased C.winbase.SYSTEMTIME;
   begin
      Local_Date := Date + Duration (Time_Zone * 60);
      Sub_Second := Duration'Fixed_Value (
         System.Native_Time.Nanosecond_Number'Integer_Value (Local_Date)
         mod 1_000_000_000);
      Local_Date := Local_Date - Sub_Second;
      FileTime := To_Native_Time (Local_Date);
      Error := C.winbase.FileTimeToSystemTime (
         FileTime'Access,
         SystemTime'Access) = 0;
      if not Error then
         Year := Year_Number (SystemTime.wYear);
         Month := Month_Number (SystemTime.wMonth);
         Day := Day_Number (SystemTime.wDay);
         Hour := Hour_Number (SystemTime.wHour);
         Minute := Minute_Number (SystemTime.wMinute);
         Second := Second_Number (SystemTime.wSecond);
         Leap_Second := False;
         Day_of_Week := (Integer (SystemTime.wDayOfWeek) + 6) rem 7;
         --  Day_Name starts from Monday
      end if;
   end Split;

   procedure Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
      Leap_Second : Boolean;
      Time_Zone : Time_Offset;
      Result : out Time;
      Error : out Boolean)
   is
      pragma Unreferenced (Leap_Second);
      SystemTime : aliased C.winbase.SYSTEMTIME := (
         wYear => C.windef.WORD (Year),
         wMonth => C.windef.WORD (Month),
         wDayOfWeek => 16#ffff#,
         wDay => C.windef.WORD (Day),
         wHour => 0,
         wMinute => 0,
         wSecond => 0,
         wMilliseconds => 0);
      FileTime : aliased C.windef.FILETIME;
   begin
      Error := C.winbase.SystemTimeToFileTime (
         SystemTime'Access,
         FileTime'Access) = 0;
      if not Error then
         Result := To_Time (FileTime) - Duration (Time_Zone * 60) + Seconds;
      end if;
   end Time_Of;

   procedure Delay_Until (T : Native_Time) is
      Timeout_T : constant Duration := System.Native_Time.To_Duration (T);
      Current_T : constant Duration := System.Native_Time.To_Duration (Clock);
      D : Duration;
   begin
      if Timeout_T > Current_T then
         D := Timeout_T - Current_T;
      else
         D := 0.0; -- always calling Delay_For for abort checking
      end if;
      System.Native_Time.Delay_For (D);
   end Delay_Until;

end System.Native_Calendar;
