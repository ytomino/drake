with Ada.Exception_Identification.From_Here;
with System.Native_Calendar;
with System.Native_Time;
package body Ada.Calendar is
   --  please use -gnato for overflow checking
   RM_9_6_26_Overflow_Check : constant Boolean := Overflow_Check'Enabled;
   --  it could not use 'Enabled in "+", "-" since Inline_Always.
   use Exception_Identification.From_Here;
   use type System.Native_Time.Nanosecond_Number;

   --  for Year, Month, Day

   type Packed_Split_Time is mod 2 ** 32;
--  for Packed_Split_Time use record
--    Day at 0 range 0 .. 7; -- 2 ** 5 = 32 > 31
--    Month at 0 range 8 .. 15; -- 2 ** 4 = 16 > 12
--    Year at 0 range 16 .. 31; -- 2 ** 9 = 512 > 2399 - 1901 + 1 = 499
--  end record;

   function Shift_Left (Value : Packed_Split_Time; Amount : Natural)
      return Packed_Split_Time
      with Import, Convention => Intrinsic;
   function Shift_Right (Value : Packed_Split_Time; Amount : Natural)
      return Packed_Split_Time
      with Import, Convention => Intrinsic;

   function Packed_Split (Date : Time) return Packed_Split_Time;
   pragma Pure_Function (Packed_Split);
   pragma Machine_Attribute (Packed_Split, "const");
   --  The callings of this function will be unified since pure attribute
   --    when Year, Month and Day are inlined

   function Packed_Split (Date : Time) return Packed_Split_Time is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : System.Native_Calendar.Hour_Number;
      Minute : System.Native_Calendar.Minute_Number;
      Second : System.Native_Calendar.Second_Number;
      Sub_Second : System.Native_Calendar.Second_Duration;
      Day_of_Week : System.Native_Calendar.Day_Name;
      Leap_Second : Boolean;
      Error : Boolean;
   begin
      System.Native_Calendar.Split (
         Duration (Date),
         Year => Year,
         Month => Month,
         Day => Day,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second,
         Leap_Second => Leap_Second,
         Day_of_Week => Day_of_Week,
         Time_Zone => 0, -- GMT
         Error => Error);
      if Error then
         Raise_Exception (Time_Error'Identity);
      end if;
      return Packed_Split_Time (Day)
         or Shift_Left (Packed_Split_Time (Month), 8)
         or Shift_Left (Packed_Split_Time (Year), 16);
   end Packed_Split;

   --  implementation

   function Clock return Time is
   begin
      return Time (
         System.Native_Calendar.To_Time (System.Native_Calendar.Clock));
   end Clock;

   function Year (Date : Time) return Year_Number is
      pragma Suppress (Range_Check);
   begin
      return Year_Number (Shift_Right (Packed_Split (Date), 16));
   end Year;

   function Month (Date : Time) return Month_Number is
      pragma Suppress (Range_Check);
   begin
      return Month_Number (Shift_Right (Packed_Split (Date), 8) and 16#ff#);
   end Month;

   function Day (Date : Time) return Day_Number is
      pragma Suppress (Range_Check);
   begin
      return Day_Number (Packed_Split (Date) and 16#ff#);
   end Day;

   function Seconds (Date : Time) return Day_Duration is
      pragma Suppress (Range_Check);
   begin
      return Duration'Fixed_Value (
         System.Native_Time.Nanosecond_Number'Integer_Value (Date)
         mod (24 * 60 * 60 * 1000000000));
   end Seconds;

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Seconds : out Day_Duration)
   is
      pragma Suppress (Range_Check);
      Hour : System.Native_Calendar.Hour_Number;
      Minute : System.Native_Calendar.Minute_Number;
      Second : System.Native_Calendar.Second_Number;
      Sub_Second : System.Native_Calendar.Second_Duration;
      Day_of_Week : System.Native_Calendar.Day_Name;
      Leap_Second : Boolean;
      Error : Boolean;
   begin
      System.Native_Calendar.Split (
         Duration (Date),
         Year => Year,
         Month => Month,
         Day => Day,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second,
         Leap_Second => Leap_Second,
         Day_of_Week => Day_of_Week,
         Time_Zone => 0, -- GMT
         Error => Error);
      if Error then
         Raise_Exception (Time_Error'Identity);
      end if;
      Seconds := Duration ((Hour * 60 + Minute) * 60 + Second) + Sub_Second;
   end Split;

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration := 0.0)
      return Time
   is
      Result : Duration;
      Error : Boolean;
   begin
      System.Native_Calendar.Time_Of (
         Year => Year,
         Month => Month,
         Day => Day,
         Seconds => Seconds,
         Leap_Second => False,
         Time_Zone => 0,
         Result => Result,
         Error => Error);
      if Error then
         Raise_Exception (Time_Error'Identity);
      end if;
      return Time (Result);
   end Time_Of;

   function "+" (Left : Time; Right : Duration) return Time is
   begin
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check then
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
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check then
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
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check then
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
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check then
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
