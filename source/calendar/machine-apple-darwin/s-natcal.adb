with System.Native_Time;
with C.sys.time;
package body System.Native_Calendar is
--  use type C.signed_int;
--  use type C.sys.types.time_t;

   Diff : constant := 5680281600.0;
      --  seconds from 1970-01-01 (0 of POSIX time)
      --    to 2150-01-01 (0 of Ada time)

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
   begin
      return System.Native_Time.To_timespec (T + Diff);
   end To_Native_Time;

   function To_Time (T : Native_Time) return Duration is
   begin
      return System.Native_Time.To_Duration (T) - Diff;
   end To_Time;

   function To_Time (T : C.sys.types.time_t) return Duration is
   begin
      return System.Native_Time.To_Duration (T) - Diff;
   end To_Time;

   function Clock return Native_Time is
      use type C.signed_int;
      Result : aliased C.sys.time.struct_timeval;
      R : C.signed_int;
   begin
      R := C.sys.time.gettimeofday (Result'Access, null);
      if R < 0 then
         raise Program_Error; -- ???
      end if;
      return System.Native_Time.To_timespec (Result);
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
      use type C.sys.types.time_t;
      timespec : aliased C.time.struct_timespec := To_Native_Time (Date);
      Buffer : aliased C.time.struct_tm := (others => <>); -- uninitialized
      tm : access C.time.struct_tm;
   begin
      Sub_Second :=
         Duration'Fixed_Value (
            System.Native_Time.Nanosecond_Number (timespec.tv_nsec));
      timespec.tv_sec := timespec.tv_sec + C.sys.types.time_t (Time_Zone) * 60;
      tm := C.time.gmtime_r (timespec.tv_sec'Access, Buffer'Access);
      if tm = null then
         Error := True;
      else
         Year := Integer (tm.tm_year) + 1900;
         Month := Integer (tm.tm_mon) + 1;
         Day := Day_Number (tm.tm_mday);
         Hour := Hour_Number (tm.tm_hour);
         Minute := Minute_Number (tm.tm_min);
         Second := Second_Number (tm.tm_sec);
         Leap_Second := False;
         Day_of_Week := (Integer (tm.tm_wday) + 6) rem 7; -- starts from Monday
         Error := False;
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
      use type C.sys.types.time_t;
      tm : aliased C.time.struct_tm := (
         tm_sec => 0,
         tm_min => 0,
         tm_hour => 0,
         tm_mday => C.signed_int (Day),
         tm_mon => C.signed_int (Month_Number'Base (Month) - 1),
         tm_year => C.signed_int (Year_Number'Base (Year) - 1900),
         tm_wday => 0,
         tm_yday => 0,
         tm_isdst => 0,
         tm_gmtoff => 0,
         tm_zone => null);
      time : C.sys.types.time_t;
   begin
      time := C.time.timegm (tm'Access);
      Error := False;
      --  UNIX time starts until 1970, Year_Number stats unitl 1901...
      if time = -1 then -- to pass negative UNIX time (?)
         if Year = 1901 and then Month = 1 and then Day = 1 then
            Result := -7857734400.0; -- first day in Time
         else
            Error := True;
         end if;
      else
         Result := To_Time (time);
      end if;
      if not Error then
         Result := Result - Duration (Time_Zone * 60) + Seconds;
      end if;
   end Time_Of;

   procedure Simple_Delay_Until (T : Native_Time) is
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
   end Simple_Delay_Until;

   procedure Delay_Until (T : Native_Time) is
   begin
      Delay_Until_Hook.all (T);
   end Delay_Until;

end System.Native_Calendar;
