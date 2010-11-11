with Ada.Calendar.Inside;
package body Ada.Calendar is
   pragma Suppress (All_Checks);

   function Clock return Time
      renames Inside.Clock;

   function Year (Date : Time) return Year_Number is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Year;
   end Year;

   function Month (Date : Time) return Month_Number is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Month;
   end Month;

   function Day (Date : Time) return Day_Number is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Day;
   end Day;

   function Seconds (Date : Time) return Day_Duration is
   begin
      return Inside.Seconds (Date, Time_Zone => 0);
   end Seconds;

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Seconds : out Day_Duration)
   is
      Hour : Inside.Hour_Number;
      Minute : Inside.Minute_Number;
      Second : Inside.Second_Number;
      Sub_Second : Inside.Second_Duration;
      Day_of_Week : Inside.Day_Name;
      Leap_Second : Boolean;
   begin
      Inside.Split (
         Date,
         Year => Year,
         Month => Month,
         Day => Day,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second,
         Leap_Second => Leap_Second,
         Day_of_Week => Day_of_Week,
         Time_Zone => 0); -- GMT
      Seconds := Duration ((Hour * 60 + Minute) * 60 + Second) + Sub_Second;
   end Split;

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration := 0.0)
      return Time is
   begin
      return Inside.Time_Of (
         Year => Year,
         Month => Month,
         Day => Day,
         Seconds => Seconds,
         Leap_Second => False,
         Time_Zone => 0);
   end Time_Of;

   function "+" (Left : Time; Right : Duration) return Time is
   begin
      return Time (Duration (Left) + Right);
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
   begin
      return Time (Left + Duration (Right));
   end "+";

   function "-" (Left : Time; Right : Duration) return Time is
   begin
      return Time (Duration (Left) - Right);
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
   begin
      return Duration (Left) - Duration (Right);
   end "-";

end Ada.Calendar;
