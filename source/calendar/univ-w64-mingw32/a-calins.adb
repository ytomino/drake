with Ada.Exceptions;
with System.Native_Time;
with C.winbase;
with C.windef;
package body Ada.Calendar.Inside is
   pragma Suppress (All_Checks);
   use type System.Native_Time.Nanosecond_Number;
   use type C.windef.WINBOOL;

   --  implementation

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
      Time_Zone : Time_Offset)
   is
      Local_Date : Duration;
      FileTime : aliased System.Native_Time.Native_Time;
      SystemTime : aliased C.winbase.SYSTEMTIME;
   begin
      Local_Date := Duration (Date) + Duration (Time_Zone * 60);
      Sub_Second := Duration'Fixed_Value (
         System.Native_Time.Nanosecond_Number'Integer_Value (Local_Date)
         rem 1000_000_000);
      Local_Date := Local_Date - Sub_Second;
      FileTime := System.Native_Time.To_Native_Time (Local_Date);
      if C.winbase.FileTimeToSystemTime (
         FileTime'Access,
         SystemTime'Access) = 0
      then
         Exceptions.Raise_Exception_From_Here (Time_Error'Identity);
      else
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

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
      Leap_Second : Boolean;
      Time_Zone : Time_Offset)
      return Time
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
      if C.winbase.SystemTimeToFileTime (
         SystemTime'Access,
         FileTime'Access) = 0
      then
         Exceptions.Raise_Exception_From_Here (Time_Error'Identity);
      else
         return Time (
            System.Native_Time.To_Time (FileTime)
            - Duration (Time_Zone * 60)
            + Seconds);
      end if;
   end Time_Of;

end Ada.Calendar.Inside;
