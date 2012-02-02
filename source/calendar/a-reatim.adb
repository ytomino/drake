package body Ada.Real_Time is
   pragma Suppress (All_Checks);

   --  implementation

   function Clock return Time is
   begin
      return Time (System.Native_Time.To_Time (System.Native_Time.Clock));
   end Clock;

   function Microseconds (US : Integer) return Time_Span is
   begin
      return US * 0.000_001;
   end Microseconds;

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      return MS * 0.001;
   end Milliseconds;

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      return NS * 0.000_000_001;
   end Nanoseconds;

   function Seconds (S : Integer) return Time_Span is
   begin
      return Time_Span (S);
   end Seconds;

   function "abs" (Right : Time_Span) return Time_Span is
   begin
      return Time_Span (abs Duration (Right));
   end "abs";

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      return Time (Duration (Left) + Duration (Right));
   end "+";

   function "+" (Left : Time_Span; Right : Time) return Time is
   begin
      return Time (Duration (Left) + Duration (Right));
   end "+";

   function "+" (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Duration (Left) + Duration (Right));
   end "+";

   function "-" (Left : Time; Right : Time_Span) return Time is
   begin
      return Time (Duration (Left) - Duration (Right));
   end "-";

   function "-" (Left : Time; Right : Time) return Time_Span is
   begin
      return Time_Span (Duration (Left) - Duration (Right));
   end "-";

   function "-" (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Duration (Left) - Duration (Right));
   end "-";

   function "-" (Right : Time_Span) return Time_Span is
   begin
      return Time_Span (-Duration (Right));
   end "-";

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Duration (Left) * Right);
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Left * Duration (Right));
   end "*";

   function "/" (Left, Right : Time_Span) return Integer is
   begin
      return Integer (Duration (Left) / Duration (Right));
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Duration (Left) / Right);
   end "/";

   function "<" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) < Duration (Right);
   end "<";

   function "<" (Left, Right : Time_Span) return Boolean is
   begin
      return Duration (Left) < Duration (Right);
   end "<";

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) <= Duration (Right);
   end "<=";

   function "<=" (Left, Right : Time_Span) return Boolean is
   begin
      return Duration (Left) <= Duration (Right);
   end "<=";

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) > Duration (Right);
   end ">";

   function ">" (Left, Right : Time_Span) return Boolean is
   begin
      return Duration (Left) > Duration (Right);
   end ">";

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) >= Duration (Right);
   end ">=";

   function ">=" (Left, Right : Time_Span) return Boolean is
   begin
      return Duration (Left) >= Duration (Right);
   end ">=";

end Ada.Real_Time;
