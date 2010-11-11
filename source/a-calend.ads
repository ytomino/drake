pragma License (Unrestricted);
package Ada.Calendar is

   type Time is private;

   subtype Year_Number is Integer range 1901 .. 2399;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number is Integer range 1 .. 31;
   subtype Day_Duration is Duration range 0.0 .. 86_400.0;

   function Clock return Time;

   function Year (Date : Time) return Year_Number;
   function Month (Date : Time) return Month_Number;
   function Day (Date : Time) return Day_Number;
   function Seconds (Date : Time) return Day_Duration;

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Seconds : out Day_Duration);

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration := 0.0)
      return Time;

   function "+" (Left : Time; Right : Duration) return Time;
   function "+" (Left : Duration; Right : Time) return Time;
   pragma Convention (Intrinsic, "+");
   pragma Pure_Function ("+");
   pragma Inline_Always ("+");
   function "-" (Left : Time; Right : Duration) return Time;
   function "-" (Left : Time; Right : Time) return Duration;
   pragma Convention (Intrinsic, "-");
   pragma Pure_Function ("-");
   pragma Inline_Always ("-");

   function "<" (Left, Right : Time) return Boolean;
   pragma Import (Intrinsic, "<");
   function "<=" (Left, Right : Time) return Boolean;
   pragma Import (Intrinsic, "<=");
   function ">" (Left, Right : Time) return Boolean;
   pragma Import (Intrinsic, ">");
   function ">=" (Left, Right : Time) return Boolean;
   pragma Import (Intrinsic, ">=");

   Time_Error : exception;

private

   type Time is new Duration; --  0 = 2150-01-01 00:00:00

end Ada.Calendar;
