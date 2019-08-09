pragma License (Unrestricted);
package Ada.Calendar.Arithmetic is

   --  Arithmetic on days:

   type Day_Count is range
      -366 * (1 + Year_Number'Last - Year_Number'First) ..
      +366 * (1 + Year_Number'Last - Year_Number'First);

   subtype Leap_Seconds_Count is Integer range -2047 .. 2047;

   procedure Difference (
      Left, Right : Time;
      Days : out Day_Count;
      Seconds : out Duration;
      Leap_Seconds : out Leap_Seconds_Count);

   function "+" (Left : Time; Right : Day_Count) return Time;
   function "+" (Left : Day_Count; Right : Time) return Time;
   function "-" (Left : Time; Right : Day_Count) return Time;
   function "-" (Left, Right : Time) return Day_Count;

   pragma Inline ("+");
   pragma Inline ("-");

end Ada.Calendar.Arithmetic;
