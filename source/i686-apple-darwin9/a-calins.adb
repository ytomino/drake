with Ada.Unchecked_Conversion;
with C.time;
package body Ada.Calendar.Inside is
   pragma Suppress (All_Checks);
   use type C.signed_int; --  time_t is signed int or signed long
   use type C.signed_long;

   type Time_Rep is range
      -(2 ** (Time'Size - 1)) ..
      +(2 ** (Time'Size - 1)) - 1;
   for Time_Rep'Size use Time'Size;

   function Clock return Time is
      Result : aliased C.sys.time.struct_timeval;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.sys.time.gettimeofday (Result'Access, null);
      return To_Time (Result);
   end Clock;

   function Seconds (Date : Time; Time_Zone : Time_Offset)
      return Day_Duration
   is
      function Cast is new Unchecked_Conversion (Time, Time_Rep);
      function Cast is new Unchecked_Conversion (Time_Rep, Duration);
   begin
      return Cast ((Cast (Date) + Time_Rep (Time_Zone) * (60 * 1000000000)) mod
         (24 * 60 * 60 * 1000000000));
   end Seconds;

   procedure Split (
      Seconds : Duration;
      Hour : out Natural;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration)
   is
      function Cast is new Unchecked_Conversion (Duration, Time_Rep);
      function Cast is new Unchecked_Conversion (Time_Rep, Duration);
      X : Time_Rep := Cast (Seconds); --  unit is 1-nanoscond
   begin
      Sub_Second := Cast (X rem 1000000000);
      X := (X - Cast (Sub_Second)) / 1000000000; --  unit is 1-second
      Second := Second_Number (X rem 60);
      X := (X - Time_Rep (Second)) / 60; --  unit is 1-minute
      Minute := Minute_Number (X rem 60);
      X := (X - Time_Rep (Minute)) / 60;
      Hour := Integer (X);
   end Split;

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
      C_time : aliased C.sys.types.time_t;
      Buffer : aliased C.time.struct_tm := (others => <>); --  uninitialized
      tm : access C.time.struct_tm;
   begin
      Split (Date, C_time, Sub_Second);
      C_time := C_time + C.sys.types.time_t (Time_Zone) * 60;
      tm := C.time.gmtime_r (C_time'Access, Buffer'Access);
      Year := Year_Number (tm.tm_year + 1900);
      Month := Month_Number (tm.tm_mon + 1);
      Day := Day_Number (tm.tm_mday);
      Hour := Hour_Number (tm.tm_hour);
      Minute := Minute_Number (tm.tm_min);
      Second := Second_Number (tm.tm_sec);
      Day_of_Week := Day_Name ((tm.tm_wday + 6) rem 7); --  starts from Monday
      Leap_Second := False;
   end Split;

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
      Leap_Second : Boolean := False;
      Time_Zone : Time_Offset)
      return Time
   is
      pragma Unreferenced (Leap_Second);
      tm : aliased C.time.struct_tm := (
         tm_sec => 0,
         tm_min => 0,
         tm_hour => 0,
         tm_mday => C.signed_int (Day),
         tm_mon => C.signed_int (Month) - 1,
         tm_year => C.signed_int (Year) - 1900,
         tm_wday => 0,
         tm_yday => 0,
         tm_isdst => 0,
         tm_gmtoff => 0,
         tm_zone => null);
      C_Result : constant C.sys.types.time_t := C.time.timegm (tm'Access);
      Result : Time;
   begin
      --  UNIX time starts until 1970, Year_Number stats unitl 1901...
      if C_Result = -1 then
         if Year = 1901 and then Month = 1 and then Day = 1 then
            Result := -7857734400.0; -- first day in Time
         else
            raise Time_Error;
         end if;
      else
         Result := To_Time (C_Result);
      end if;
      return Result - Duration (Time_Zone * 60) + Seconds;
   end Time_Of;

   procedure Delay_For (D : Duration) is
   begin
      --  tasking feature is unimplemented
      if D > 0.0 then
         declare
            T : aliased C.sys.time.struct_timespec := To_timespec (D);
            Dummy : C.signed_int;
            pragma Unreferenced (Dummy);
         begin
            Dummy := C.time.nanosleep (T'Access, null);
         end;
      end if;
   end Delay_For;

   procedure Delay_Until (T : Time) is
   begin
      --  tasking feature is unimplemented
      Delay_For (T - Clock);
   end Delay_Until;

   --  type Duration is delta 0.000000001 range ... (1-nanosecond)
   --  The unit of struct timespec (tv_nsec) is same 1-nanosecond.
   --  The unit of struct timeval (tv_usec) is same 1-microsecond.

   Diff : constant Time_Rep := -5680281600; -- 1971-01-01 - 2150-01-01

   function To_Time (T : C.sys.types.time_t) return Time is
      function Cast is new Unchecked_Conversion (Time_Rep, Time);
   begin
      return Cast ((Time_Rep (T) + Diff) * 1000000000);
   end To_Time;

   function To_Time (T : C.sys.time.struct_timespec) return Time is
      function Cast is new Unchecked_Conversion (Time_Rep, Time);
   begin
      return Cast ((Time_Rep (T.tv_sec) + Diff) * 1000000000 +
         Time_Rep (T.tv_nsec));
   end To_Time;

   function To_Time (T : C.sys.time.struct_timeval) return Time is
      function Cast is new Unchecked_Conversion (Time_Rep, Time);
   begin
      return Cast ((Time_Rep (T.tv_sec) + Diff) * 1000000000 +
         Time_Rep (T.tv_usec) * 1000);
   end To_Time;

   procedure Split (
      Date : Time;
      Result : out C.sys.types.time_t;
      Sub_Second : out Second_Duration)
   is
      function Cast is new Unchecked_Conversion (Time, Time_Rep);
      function Cast is new Unchecked_Conversion (Duration, Time_Rep);
      function Cast is new Unchecked_Conversion (Time_Rep, Duration);
   begin
      Sub_Second := Cast (Cast (Date) mod 1000000000);
      --  Under_Second >= 0
      Result := C.sys.types.time_t (
         Time_Rep'(Cast (Date) - Cast (Sub_Second)) / 1000000000 - Diff);
   end Split;

   function To_timespec (D : Duration) return C.sys.time.struct_timespec is
      function Cast is new Unchecked_Conversion (Duration, Time_Rep);
      Sub_Second : constant Time_Rep := Cast (D) mod 1000000000;
   begin
      return (
         tv_sec => C.sys.types.time_t ((Cast (D) - Sub_Second) / 1000000000),
         tv_nsec => C.signed_long (Sub_Second));
   end To_timespec;

end Ada.Calendar.Inside;
