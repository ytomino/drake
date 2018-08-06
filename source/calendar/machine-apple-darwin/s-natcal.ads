pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with C.time;
package System.Native_Calendar is
   pragma Preelaborate;

   subtype Native_Time is C.time.struct_timespec;

   function To_Native_Time (T : Duration) return Native_Time;
   function To_Time (T : Native_Time) return Duration;

   pragma Pure_Function (To_Native_Time);
   pragma Pure_Function (To_Time);

   function Clock return Native_Time;

   --  same as Ada.Calendar

   subtype Time is Duration;

   subtype Year_Number is Integer range 1901 .. 2399;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number is Integer range 1 .. 31;
   subtype Day_Duration is Duration range 0.0 .. 86_400.0;

   --  same as Ada.Calendar.Formatting

   subtype Time_Offset is Integer range -28 * 60 .. 28 * 60;

   subtype Day_Name is Integer range 0 .. 6;

   subtype Hour_Number is Natural range 0 .. 23;
   subtype Minute_Number is Natural range 0 .. 59;
   subtype Second_Number is Natural range 0 .. 59;
   subtype Second_Duration is Day_Duration range 0.0 .. 1.0;

   --  decomposing/composing

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
      Error : out Boolean);

   procedure Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
      Leap_Second : Boolean;
      Time_Zone : Time_Offset;
      Result : out Time;
      Error : out Boolean);

   --  for Time_Zones

   procedure UTC_Time_Offset (
      Date : Time;
      Time_Zone : out Time_Offset;
      Error : out Boolean);

   procedure Initialize_Time_Zones
      renames C.time.tzset;

   --  for delay until

   procedure Simple_Delay_Until (T : Native_Time);

   type Delay_Until_Handler is access procedure (T : Native_Time);
   pragma Favor_Top_Level (Delay_Until_Handler);

   --  equivalent to Timed_Delay (s-soflin.ads)
   Delay_Until_Hook : not null Delay_Until_Handler :=
      Simple_Delay_Until'Access;

   procedure Delay_Until (T : Native_Time);

end System.Native_Calendar;
