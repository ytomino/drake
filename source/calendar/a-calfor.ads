pragma License (Unrestricted);
with Ada.Calendar.Time_Zones;
package Ada.Calendar.Formatting is

   --  Day of the week:

   type Day_Name is (
      Monday,
      Tuesday,
      Wednesday,
      Thursday,
      Friday,
      Saturday,
      Sunday);

--  function Day_of_Week (Date : Time) return Day_Name;
   --  RM defined Day_*o*f_Week, but GNAT runtime defined Day_*O*f_Week...
   function Day_Of_Week (
      Date : Time;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return Day_Name;
   pragma Pure_Function (Day_Of_Week);
   pragma Inline (Day_Of_Week);

   --  Hours:Minutes:Seconds access:

   subtype Hour_Number is Natural range 0 .. 23;
   subtype Minute_Number is Natural range 0 .. 59;
   subtype Second_Number is Natural range 0 .. 59;
   subtype Second_Duration is Day_Duration range 0.0 .. 1.0;

   function Year (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Year_Number;
   pragma Pure_Function (Year);
   pragma Inline (Year);

   function Month (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Month_Number;
   pragma Pure_Function (Month);
   pragma Inline (Month);

   function Day (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Day_Number;
   pragma Pure_Function (Day);
   pragma Inline (Day);

   function Hour (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Hour_Number;
   pragma Pure_Function (Hour);
   pragma Inline (Hour);

   function Minute (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Minute_Number;
   pragma Pure_Function (Minute);
   pragma Inline (Minute);

   function Second (Date : Time) return Second_Number;
   pragma Pure_Function (Second);
   pragma Inline (Second);

   function Sub_Second (Date : Time) return Second_Duration;
   pragma Pure_Function (Sub_Second);
   pragma Inline (Sub_Second);

   --  extended
   --  This function returns seconds in a day
   --    including the offset of the time zone.
   function Seconds (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Day_Duration;
   pragma Pure_Function (Seconds);
   pragma Inline (Seconds);

   function Seconds_Of (
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number := 0;
      Sub_Second : Second_Duration := 0.0)
      return Day_Duration;

   procedure Split (
      Seconds : Day_Duration;
      Hour : out Hour_Number;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration);

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration := 0.0;
      Leap_Second : Boolean := False;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return Time;

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration := 0.0;
      Leap_Second : Boolean := False;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return Time;

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Hour : out Hour_Number;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration;
      Time_Zone : Time_Zones.Time_Offset := 0);

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
      Time_Zone : Time_Zones.Time_Offset := 0);

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Seconds : out Day_Duration;
      Leap_Second : out Boolean;
      Time_Zone : Time_Zones.Time_Offset := 0);

   --  Simple image and value:
   function Image (
      Date : Time;
      Include_Time_Fraction : Boolean := False;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return String;

   function Value (
      Date : String;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return Time;

   function Image (
      Elapsed_Time : Duration;
      Include_Time_Fraction : Boolean := False)
      return String;

   function Value (Elapsed_Time : String) return Duration;

   --  extended
   --  The format of time-zone is "+00:00".
   function Image (Time_Zone : Time_Zones.Time_Offset) return String;
   function Value (Time_Zone : String) return Time_Zones.Time_Offset;

end Ada.Calendar.Formatting;
