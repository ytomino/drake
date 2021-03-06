pragma License (Unrestricted);
private with System.Native_Real_Time;
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

   function "+" (Left : Time; Right : Time_Span) return Time
      with Import, Convention => Intrinsic;
   function "+" (Left : Time_Span; Right : Time) return Time
      with Import, Convention => Intrinsic;
   function "-" (Left : Time; Right : Time_Span) return Time
      with Import, Convention => Intrinsic;
   function "-" (Left : Time; Right : Time) return Time_Span
      with Import, Convention => Intrinsic;

   function "<" (Left, Right : Time) return Boolean
      with Import, Convention => Intrinsic;
   function "<=" (Left, Right : Time) return Boolean
      with Import, Convention => Intrinsic;
   function ">" (Left, Right : Time) return Boolean
      with Import, Convention => Intrinsic;
   function ">=" (Left, Right : Time) return Boolean
      with Import, Convention => Intrinsic;

   function "+" (Left, Right : Time_Span) return Time_Span
      with Import, Convention => Intrinsic;
   function "-" (Left, Right : Time_Span) return Time_Span
      with Import, Convention => Intrinsic;
   function "-" (Right : Time_Span) return Time_Span
      with Import, Convention => Intrinsic;
   function "*" (Left : Time_Span; Right : Integer) return Time_Span
      with Convention => Intrinsic;
   function "*" (Left : Integer; Right : Time_Span) return Time_Span
      with Convention => Intrinsic;
   function "/" (Left, Right : Time_Span) return Integer
      with Convention => Intrinsic;
   function "/" (Left : Time_Span; Right : Integer) return Time_Span
      with Convention => Intrinsic;

   pragma Pure_Function ("*");
   pragma Pure_Function ("/");
   pragma Inline_Always ("*");
   pragma Inline_Always ("/");

   function "abs" (Right : Time_Span) return Time_Span
      with Import, Convention => Intrinsic;

   function "<" (Left, Right : Time_Span) return Boolean
      with Import, Convention => Intrinsic;
   function "<=" (Left, Right : Time_Span) return Boolean
      with Import, Convention => Intrinsic;
   function ">" (Left, Right : Time_Span) return Boolean
      with Import, Convention => Intrinsic;
   function ">=" (Left, Right : Time_Span) return Boolean
      with Import, Convention => Intrinsic;

   function To_Duration (TS : Time_Span) return Duration;
   function To_Time_Span (D : Duration) return Time_Span;

   pragma Pure_Function (To_Duration);
   pragma Pure_Function (To_Time_Span);
   pragma Inline (To_Duration);
   pragma Inline (To_Time_Span);

   function Nanoseconds (NS : Integer) return Time_Span;
   function Microseconds (US : Integer) return Time_Span;
   function Milliseconds (MS : Integer) return Time_Span;
   function Seconds (S : Integer) return Time_Span;
--  function Minutes (M : Integer) return Time_Span;

   pragma Pure_Function (Nanoseconds);
   pragma Pure_Function (Microseconds);
   pragma Pure_Function (Milliseconds);
   pragma Pure_Function (Seconds);
   pragma Inline (Nanoseconds);
   pragma Inline (Microseconds);
   pragma Inline (Milliseconds);
   pragma Inline (Seconds);

   type Seconds_Count is range
      -(2 ** (Duration'Size - 1)) / 1_000_000_000 ..
      (2 ** (Duration'Size - 1) - 1) / 1_000_000_000; -- implementation-defined

--  procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span);
--  function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time;

   --  Note: What is Split meaning?
   --    Because the origin point of Time is unspecified...

private

   type Time is new Duration;

   Time_First : constant Time := Time'First;
   Time_Last : constant Time := Time'Last;

   type Time_Span is new Duration;

   Time_Span_First : constant Time_Span := Time_Span'First;
   Time_Span_Last : constant Time_Span := Time_Span'Last;
   Time_Span_Zero : constant Time_Span := 0.0;
   Time_Span_Unit : constant Time_Span := Time_Span'Delta;

   Tick : constant Time_Span := System.Native_Real_Time.Tick;

end Ada.Real_Time;
