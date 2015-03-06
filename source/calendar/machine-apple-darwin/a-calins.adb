with Ada.Exception_Identification.From_Here;
with System.Native_Time;
with C.sys.types;
with C.time;
package body Ada.Calendar.Inside is
   pragma Suppress (All_Checks);
   use Exception_Identification.From_Here;
   use type C.signed_int; -- time_t is signed int or signed long
   use type C.signed_long;

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
      timespec : aliased System.Native_Time.Native_Time :=
         System.Native_Time.To_Native_Time (Duration (Date));
      Buffer : aliased C.time.struct_tm := (others => <>); -- uninitialized
      tm : access C.time.struct_tm;
   begin
      Sub_Second := Duration'Fixed_Value (
         System.Native_Time.Nanosecond_Number (timespec.tv_nsec));
      timespec.tv_sec := timespec.tv_sec + C.sys.types.time_t (Time_Zone) * 60;
      tm := C.time.gmtime_r (timespec.tv_sec'Access, Buffer'Access);
      --  does gmtime_r return no error ?
      Year := Integer (tm.tm_year) + 1900;
      Month := Integer (tm.tm_mon) + 1;
      Day := Day_Number (tm.tm_mday);
      Hour := Hour_Number (tm.tm_hour);
      Minute := Minute_Number (tm.tm_min);
      Second := Second_Number (tm.tm_sec);
      Leap_Second := False;
      Day_of_Week := (Integer (tm.tm_wday) + 6) rem 7; -- starts from Monday
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
      C_Result : constant C.sys.types.time_t := C.time.timegm (tm'Access);
      Result : Duration;
   begin
      --  UNIX time starts until 1970, Year_Number stats unitl 1901...
      if C_Result = -1 then -- to pass negative UNIX time (?)
         if Year = 1901 and then Month = 1 and then Day = 1 then
            Result := -7857734400.0; -- first day in Time
         else
            Raise_Exception (Time_Error'Identity);
         end if;
      else
         Result := System.Native_Time.To_Time (C_Result);
      end if;
      return Time (Result - Duration (Time_Zone * 60) + Seconds);
   end Time_Of;

end Ada.Calendar.Inside;
