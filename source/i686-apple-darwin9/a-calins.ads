pragma License (Unrestricted);
--  implementation package
with C.sys.time;
with C.sys.types;
package Ada.Calendar.Inside is

   --  implementation

   function Clock return Time;

   subtype Time_Offset is Integer range -28 * 60 .. 28 * 60;

   subtype Day_Name is Integer range 0 .. 6;

   subtype Hour_Number is Natural range 0 .. 23;
   subtype Minute_Number is Natural range 0 .. 59;
   subtype Second_Number is Natural range 0 .. 59;
   subtype Second_Duration is Day_Duration range 0.0 .. 1.0;

   function Seconds (Date : Time; Time_Zone : Time_Offset)
      return Day_Duration;

   procedure Split (
      Seconds : Duration;
      Hour : out Natural;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration);

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
      Time_Zone : Time_Offset);

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
      Leap_Second : Boolean := False;
      Time_Zone : Time_Offset)
      return Time;

   procedure Delay_For (D : Duration);
   procedure Delay_Until (T : Time);

   --  UNIX time

   function To_Time (T : C.sys.types.time_t) return Time;
   function To_Time (T : C.sys.time.struct_timespec) return Time;
   function To_Time (T : C.sys.time.struct_timeval) return Time;

   procedure Split (
      Date : Time;
      Result : out C.sys.types.time_t;
      Sub_Second : out Second_Duration);

   function To_timespec (D : Duration) return C.sys.time.struct_timespec;

end Ada.Calendar.Inside;
