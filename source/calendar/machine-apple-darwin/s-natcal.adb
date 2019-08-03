with System.Native_Time;
with C.sys.time;
with C.sys.types;
package body System.Native_Calendar is
--  use type C.signed_int;
--  use type C.signed_long; -- tm_gmtoff
--  use type C.sys.types.time_t;

   Diff : constant := 5680281600.0;
      --  seconds from 1970-01-01 (0 of POSIX time)
      --    to 2150-01-01 (0 of Ada time)

   function To_Time (T : C.sys.types.time_t) return Duration;
   function To_Time (T : C.sys.types.time_t) return Duration is
   begin
      return System.Native_Time.To_Duration (T) - Diff;
   end To_Time;

   function Is_Leap_Second (T : C.sys.types.time_t) return Boolean;
   function Is_Leap_Second (T : C.sys.types.time_t) return Boolean is
      Aliased_T : aliased C.sys.types.time_t := T;
      tm : aliased C.time.struct_tm;
      tm_r : access C.time.struct_tm;
   begin
      tm_r := C.time.gmtime_r (Aliased_T'Access, tm'Access);
      return tm_r /= null and then Second_Number'Base (tm_r.tm_sec) = 60;
   end Is_Leap_Second;

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
   begin
      return System.Native_Time.To_timespec (T + Diff);
   end To_Native_Time;

   function To_Time (T : Native_Time) return Duration is
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
      Seconds : out Day_Duration;
      Leap_Second : out Boolean;
      Time_Zone : Time_Offset;
      Error : out Boolean)
   is
      use type C.sys.types.time_t;
      timespec : aliased C.time.struct_timespec := To_Native_Time (Date);
      Buffer : aliased C.time.struct_tm := (others => <>); -- uninitialized
      tm : access C.time.struct_tm;
   begin
      tm := C.time.gmtime_r (timespec.tv_sec'Access, Buffer'Access);
      Error := tm = null;
      if not Error then
         --  Leap_Second is always calculated as GMT
         Leap_Second := Second_Number'Base (tm.tm_sec) >= 60;
         --  other units are calculated by Time_Zone
         if Time_Zone /= 0 then
            timespec.tv_sec :=
               timespec.tv_sec + C.sys.types.time_t (Time_Zone) * 60;
            tm := C.time.gmtime_r (timespec.tv_sec'Access, Buffer'Access);
            Error := tm = null;
         end if;
         if not Error then
            Year := Integer (tm.tm_year) + 1900;
            Month := Integer (tm.tm_mon) + 1;
            Day := Day_Number (tm.tm_mday);
            --  truncate to day
            tm.tm_hour := 0;
            tm.tm_min := 0;
            tm.tm_sec := 0;
            declare
               Truncated_Time : C.sys.types.time_t;
            begin
               Truncated_Time := C.time.timegm (tm);
               Error := Truncated_Time = -1;
               if not Error then
                  timespec.tv_sec := timespec.tv_sec - Truncated_Time;
                  Seconds := System.Native_Time.To_Duration (timespec);
                  if Leap_Second and then Time_Zone <= 0 then
                     Seconds := Seconds - 1.0;
                  end if;
               end if;
            end;
         end if;
      end if;
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
      Time_Zone : Time_Offset;
      Error : out Boolean)
   is
      use type C.sys.types.time_t;
      timespec : aliased C.time.struct_timespec := To_Native_Time (Date);
      Buffer : aliased C.time.struct_tm := (others => <>); -- uninitialized
      tm : access C.time.struct_tm;
   begin
      tm := C.time.gmtime_r (timespec.tv_sec'Access, Buffer'Access);
      Error := tm = null;
      if not Error then
         --  Second, Sub_Second and Leap_Second are always calculated as GMT
         if Second_Number'Base (tm.tm_sec) >= 60 then
            Second := 59;
            Leap_Second := True;
         else
            Second := Second_Number (tm.tm_sec);
            Leap_Second := False;
         end if;
         Sub_Second :=
            Duration'Fixed_Value (
               System.Native_Time.Nanosecond_Number (timespec.tv_nsec));
         --  other units are calculated by Time_Zone
         if Time_Zone /= 0 then
            if Leap_Second and then Time_Zone < 0 then
               timespec.tv_sec := timespec.tv_sec - 1;
            end if;
            timespec.tv_sec :=
               timespec.tv_sec + C.sys.types.time_t (Time_Zone) * 60;
            tm := C.time.gmtime_r (timespec.tv_sec'Access, Buffer'Access);
            Error := tm = null;
         end if;
         if not Error then
            Year := Integer (tm.tm_year) + 1900;
            Month := Integer (tm.tm_mon) + 1;
            Day := Day_Number (tm.tm_mday);
            Hour := Hour_Number (tm.tm_hour);
            Minute := Minute_Number (tm.tm_min);
            Day_of_Week := (Integer (tm.tm_wday) + 6) rem 7;
               --  Day_Name starts from Monday
         end if;
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
      Error := time = -1;
      --  UNIX time starts until 1970, Year_Number stats unitl 1901...
      if Error then -- to pass negative UNIX time (?)
         if Year = 1901 and then Month = 1 and then Day = 1 then
            Result := -7857734400.0; -- first day in Time
            Error := False;
         end if;
      else
         Result := To_Time (time);
      end if;
      if not Error then
         Result := Result - Duration (Time_Zone * 60) + Seconds;
         if Leap_Second then
            if Time_Zone <= 0 then
               Result := Result + 1.0;
            end if;
            --  checking
            Error := not Is_Leap_Second (To_Native_Time (Result).tv_sec);
         end if;
      end if;
   end Time_Of;

   procedure Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;
      Time_Zone : Time_Offset;
      Result : out Time;
      Error : out Boolean)
   is
      use type C.sys.types.time_t;
      tm : aliased C.time.struct_tm := (
         tm_sec => C.signed_int (Second),
         tm_min => C.signed_int (Minute),
         tm_hour => C.signed_int (Hour),
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
      Error := False;
      if Time_Zone /= 0 then
         time := C.time.timegm (tm'Access);
         Error := time = -1;
         if not Error then
            declare
               GMT : aliased C.sys.types.time_t;
               tm_r : access C.time.struct_tm;
            begin
               GMT := time - C.sys.types.time_t (Time_Zone * 60);
               tm_r := C.time.gmtime_r (GMT'Access, tm'Access);
               Error := tm_r = null;
            end;
         end if;
      end if;
      if not Error then
         if Leap_Second and then Second_Number'Base (tm.tm_sec) = 59 then
            tm.tm_sec := 60;
         end if;
         time := C.time.timegm (tm'Access);
         Error := time = -1;
         if not Error and Leap_Second then
            --  checking
            Error := not Is_Leap_Second (time);
         end if;
      end if;
      --  UNIX time starts until 1970, Year_Number stats unitl 1901...
      if Error then -- to pass negative UNIX time (?)
         if Year = 1901 and then Month = 1 and then Day = 1 then
            Result :=
               -7857734400.0 -- first day in Time
               + Duration (((Hour * 60 + Minute) * 60) + Second);
            Error := False;
         end if;
      else
         Result := To_Time (time);
      end if;
      if not Error then
         Result := Result + Sub_Second;
      end if;
   end Time_Of;

   procedure UTC_Time_Offset (
      Date : Time;
      Time_Zone : out Time_Offset;
      Error : out Boolean)
   is
      use type C.signed_long; -- tm_gmtoff
      --  FreeBSD does not have timezone variable
      GMT_Time : aliased constant Native_Time :=
         To_Native_Time (Duration (Date));
      Local_TM_Buf : aliased C.time.struct_tm;
      Local_TM : access C.time.struct_tm;
   begin
      Local_TM := C.time.localtime_r (
         GMT_Time.tv_sec'Access,
         Local_TM_Buf'Access);
      Error := Local_TM = null;
      if not Error then
         Time_Zone := Time_Offset (Local_TM.tm_gmtoff / 60);
      end if;
   end UTC_Time_Offset;

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
