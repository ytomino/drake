private with System.Native_Execution_Time;
package body Ada.Execution_Time is

   function Clock return CPU_Time is
   begin
      return To_Time_Span (System.Native_Execution_Time.Clock);
   end Clock;

end Ada.Execution_Time;
