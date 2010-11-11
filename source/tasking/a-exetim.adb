with Ada.Execution_Time.Inside;
package body Ada.Execution_Time is

   function Clock return CPU_Time
      renames Inside.Clock;

   function "-" (Left : CPU_Time; Right : CPU_Time)
      return Real_Time.Time_Span is
   begin
      return Real_Time."-" (Real_Time.Time (Left), Real_Time.Time (Right));
   end "-";

end Ada.Execution_Time;
