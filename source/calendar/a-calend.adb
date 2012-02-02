with Ada.Calendar.Inside;
with System.Native_Time;
package body Ada.Calendar is
   --  please use -gnato for overflow checking
   RM_9_6_26_Overflow_Check : constant := Boolean'Pos (
      Overflow_Check'Enabled /= False);
   --  it could not use 'Enabled in "+", "-" since Inline_Always.
   pragma Warnings (Off, RM_9_6_26_Overflow_Check);
   --  [gcc 4.5/4.6] condition is always False/True
   pragma Suppress (All_Checks);

   function Clock return Time is
   begin
      return Time (System.Native_Time.To_Time (System.Native_Time.Clock));
   end Clock;

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
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check /= 0 then
         if (Right > 0.0 and then Duration (Left) > Duration'Last - Right)
            or else (Right < 0.0
               and then Duration (Left) < Duration'First - Right)
         then
            raise Time_Error;
         end if;
      end if;
      return Time (Duration (Left) + Right);
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
   begin
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check /= 0 then
         if (Right > 0.0 and then Left > Duration'Last - Duration (Right))
            or else (Right < 0.0
               and then Left < Duration'First - Duration (Right))
         then
            raise Time_Error;
         end if;
      end if;
      return Time (Left + Duration (Right));
   end "+";

   function "-" (Left : Time; Right : Duration) return Time is
   begin
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check /= 0 then
         if (Right > 0.0 and then Duration (Left) < Duration'First + Right)
            or else (Right < 0.0
               and then Duration (Left) > Duration'Last + Right)
         then
            raise Time_Error;
         end if;
      end if;
      return Time (Duration (Left) - Right);
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
   begin
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check /= 0 then
         if (Right > 0.0
            and then Duration (Left) < Duration'First + Duration (Right))
            or else (Right < 0.0
               and then Duration (Left) > Duration'Last + Duration (Right))
         then
            raise Time_Error;
         end if;
      end if;
      return Duration (Left) - Duration (Right);
   end "-";

end Ada.Calendar;
