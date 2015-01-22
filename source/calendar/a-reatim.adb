package body Ada.Real_Time is
   pragma Suppress (All_Checks);
   use type System.Native_Time.Nanosecond_Number;

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

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Duration (Left) * Right);
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Left * Duration (Right));
   end "*";

   function "/" (Left, Right : Time_Span) return Integer is
      --  not to depend on System.Arith_64
      Left_Rep : constant System.Native_Time.Nanosecond_Number :=
         System.Native_Time.Nanosecond_Number'Integer_Value (Left);
      Right_Rep : constant System.Native_Time.Nanosecond_Number :=
         System.Native_Time.Nanosecond_Number'Integer_Value (Right);
   begin
      return Integer (Left_Rep / Right_Rep);
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Duration (Left) / Right);
   end "/";

   function To_Duration (TS : Time_Span) return Duration is
   begin
      return Duration (TS);
   end To_Duration;

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      return Time_Span (D);
   end To_Time_Span;

end Ada.Real_Time;
