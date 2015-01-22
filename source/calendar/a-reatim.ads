pragma License (Unrestricted);
private with System.Native_Time;
package Ada.Real_Time is

   type Time is private;
   Time_First : constant Time;
   Time_Last : constant Time;
   Time_Unit : constant :=
      Duration'Delta; -- implementation-defined-real-number

   type Time_Span is private;
   Time_Span_First : constant Time_Span;
   Time_Span_Last : constant Time_Span;
   Time_Span_Zero : constant Time_Span;
   Time_Span_Unit : constant Time_Span;

   Tick : constant Time_Span;
   function Clock return Time;
   pragma Inline (Clock);

   function "+" (Left : Time; Right : Time_Span) return Time
      with Pure_Function, Import, Convention => Intrinsic;
   function "+" (Left : Time_Span; Right : Time) return Time
      with Pure_Function, Import, Convention => Intrinsic;
   function "-" (Left : Time; Right : Time_Span) return Time
      with Pure_Function, Import, Convention => Intrinsic;
   function "-" (Left : Time; Right : Time) return Time_Span
      with Pure_Function, Import, Convention => Intrinsic;

   function "<" (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">" (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   function "+" (Left, Right : Time_Span) return Time_Span
      with Pure_Function, Import, Convention => Intrinsic;
   function "-" (Left, Right : Time_Span) return Time_Span
      with Pure_Function, Import, Convention => Intrinsic;
   function "-" (Right : Time_Span) return Time_Span
      with Pure_Function, Import, Convention => Intrinsic;
   function "*" (Left : Time_Span; Right : Integer) return Time_Span;
   function "*" (Left : Integer; Right : Time_Span) return Time_Span;
   pragma Pure_Function ("*");
   pragma Inline ("*");
   function "/" (Left, Right : Time_Span) return Integer;
   function "/" (Left : Time_Span; Right : Integer) return Time_Span;
   pragma Pure_Function ("/");
   pragma Inline ("/");

   function "abs" (Right : Time_Span) return Time_Span
      with Pure_Function, Import, Convention => Intrinsic;

   function "<" (Left, Right : Time_Span) return Boolean;
   function "<=" (Left, Right : Time_Span) return Boolean;
   function ">" (Left, Right : Time_Span) return Boolean;
   function ">=" (Left, Right : Time_Span) return Boolean;

   function To_Duration (TS : Time_Span) return Duration;
   pragma Pure_Function (To_Duration);
   pragma Inline (To_Duration);
   function To_Time_Span (D : Duration) return Time_Span;
   pragma Pure_Function (To_Time_Span);
   pragma Inline (To_Time_Span);

   function Nanoseconds (NS : Integer) return Time_Span;
   pragma Pure_Function (Nanoseconds);
   pragma Inline (Nanoseconds);
   function Microseconds (US : Integer) return Time_Span;
   pragma Pure_Function (Microseconds);
   pragma Inline (Microseconds);
   function Milliseconds (MS : Integer) return Time_Span;
   pragma Pure_Function (Milliseconds);
   pragma Inline (Milliseconds);
   function Seconds (S : Integer) return Time_Span;
   pragma Pure_Function (Seconds);
   pragma Inline (Seconds);
--  function Minutes (M : Integer) return Time_Span;

   type Seconds_Count is range
      -(2 ** (Duration'Size - 1)) / 1000000000 ..
      +(2 ** (Duration'Size - 1) - 1) / 1000000000; -- implementation-defined

   --  what is Split meaning? because origin point of Time is unspecified...
--  procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span);
--  function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

private

   type Time is new Duration;

   Time_First : constant Time := Time'First;
   Time_Last : constant Time := Time'Last;

   type Time_Span is new Duration;

   Time_Span_First : constant Time_Span := Time_Span'First;
   Time_Span_Last : constant Time_Span := Time_Span'Last;
   Time_Span_Zero : constant Time_Span := 0.0;
   Time_Span_Unit : constant Time_Span := Time_Span'Delta;

   Tick : constant Time_Span := System.Native_Time.Tick;

end Ada.Real_Time;
