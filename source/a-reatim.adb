package body Ada.Real_Time is
   pragma Suppress (All_Checks);

   function Clock return Time is
   begin
      return Time (Calendar.Clock);
   end Clock;

   function "-" (Left : Time; Right : Time) return Time_Span is
   begin
      return Time_Span (Calendar."-" (
         Calendar.Time (Left),
         Calendar.Time (Right)));
   end "-";

end Ada.Real_Time;
