with Ada.Calendar.Inside;
with System.Native_Time;
package body Ada.Calendar is
   --  please use -gnato for overflow checking
   RM_9_6_26_Overflow_Check : constant Boolean := Overflow_Check'Enabled;
   --  it could not use 'Enabled in "+", "-" since Inline_Always.
   pragma Warnings (Off, RM_9_6_26_Overflow_Check);
   --  [gcc 4.5/4.6] condition is always False/True
   pragma Suppress (All_Checks);
   use type System.Native_Time.Nanosecond_Number;

   --  for Year, Month, Day

   type Packed_Split_Time is mod 2 ** 32;
--  for Packed_Split_Time use record
--    Day at 0 range 0 .. 7; -- 2 ** 5 = 32 > 31
--    Month at 0 range 8 .. 15; -- 2 ** 4 = 16 > 12
--    Year at 0 range 16 .. 31; -- 2 ** 9 = 512 > 2399 - 1901 + 1 = 499
--  end record;

   function Shift_Left (Value : Packed_Split_Time; Amount : Natural)
      return Packed_Split_Time;
   pragma Import (Intrinsic, Shift_Left);
   function Shift_Right (Value : Packed_Split_Time; Amount : Natural)
      return Packed_Split_Time;
   pragma Import (Intrinsic, Shift_Right);

   function Packed_Split (Date : Time) return Packed_Split_Time;
   pragma Pure_Function (Packed_Split);
   pragma Machine_Attribute (Packed_Split, "pure");
   --  The callings of this function will be unified since pure attribute
   --    when Year, Month and Day are inlined

   function Packed_Split (Date : Time) return Packed_Split_Time is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
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
      return Packed_Split_Time (Day)
         or Shift_Left (Packed_Split_Time (Month), 8)
         or Shift_Left (Packed_Split_Time (Year), 16);
   end Packed_Split;

   --  implementation

   function Clock return Time is
   begin
      return Time (System.Native_Time.To_Time (System.Native_Time.Clock));
   end Clock;

   function Year (Date : Time) return Year_Number is
   begin
      return Year_Number (Shift_Right (Packed_Split (Date), 16));
   end Year;

   function Month (Date : Time) return Month_Number is
   begin
      return Month_Number (Shift_Right (Packed_Split (Date), 8) and 16#ff#);
   end Month;

   function Day (Date : Time) return Day_Number is
   begin
      return Day_Number (Packed_Split (Date) and 16#ff#);
   end Day;

   function Seconds (Date : Time) return Day_Duration is
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
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check then
         declare
            function add_overflow (
               a, b : System.Native_Time.Nanosecond_Number;
               res : not null access System.Native_Time.Nanosecond_Number)
               return Boolean
               with Import,
                  Convention => Intrinsic,
                  External_Name => "__builtin_saddll_overflow";
            Result : aliased System.Native_Time.Nanosecond_Number;
         begin
            if add_overflow (
               System.Native_Time.Nanosecond_Number'Integer_Value (Left),
               System.Native_Time.Nanosecond_Number'Integer_Value (Right),
               Result'Access)
            then
               raise Time_Error;
            end if;
            return Time'Fixed_Value (Result);
         end;
      else
         return Time (Duration (Left) + Right);
      end if;
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
   begin
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check then
         declare
            function add_overflow (
               a, b : System.Native_Time.Nanosecond_Number;
               res : not null access System.Native_Time.Nanosecond_Number)
               return Boolean
               with Import,
                  Convention => Intrinsic,
                  External_Name => "__builtin_saddll_overflow";
            Result : aliased System.Native_Time.Nanosecond_Number;
         begin
            if add_overflow (
               System.Native_Time.Nanosecond_Number'Integer_Value (Left),
               System.Native_Time.Nanosecond_Number'Integer_Value (Right),
               Result'Access)
            then
               raise Time_Error;
            end if;
            return Time'Fixed_Value (Result);
         end;
      else
         return Time (Left + Duration (Right));
      end if;
   end "+";

   function "-" (Left : Time; Right : Duration) return Time is
   begin
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check then
         declare
            function sub_overflow (
               a, b : System.Native_Time.Nanosecond_Number;
               res : not null access System.Native_Time.Nanosecond_Number)
               return Boolean
               with Import,
                  Convention => Intrinsic,
                  External_Name => "__builtin_ssubll_overflow";
            Result : aliased System.Native_Time.Nanosecond_Number;
         begin
            if sub_overflow (
               System.Native_Time.Nanosecond_Number'Integer_Value (Left),
               System.Native_Time.Nanosecond_Number'Integer_Value (Right),
               Result'Access)
            then
               raise Time_Error;
            end if;
            return Time'Fixed_Value (Result);
         end;
      else
         return Time (Duration (Left) - Right);
      end if;
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
   begin
      if not Standard'Fast_Math and then RM_9_6_26_Overflow_Check then
         declare
            function sub_overflow (
               a, b : System.Native_Time.Nanosecond_Number;
               res : not null access System.Native_Time.Nanosecond_Number)
               return Boolean
               with Import,
                  Convention => Intrinsic,
                  External_Name => "__builtin_ssubll_overflow";
            Result : aliased System.Native_Time.Nanosecond_Number;
         begin
            if sub_overflow (
               System.Native_Time.Nanosecond_Number'Integer_Value (Left),
               System.Native_Time.Nanosecond_Number'Integer_Value (Right),
               Result'Access)
            then
               raise Time_Error;
            end if;
            return Duration'Fixed_Value (Result);
         end;
      else
         return Duration (Left) - Duration (Right);
      end if;
   end "-";

end Ada.Calendar;
