with Ada.Execution_Time.Inside;
package body Ada.Execution_Time is
   pragma Suppress (All_Checks);

   function Clock return CPU_Time
      renames Inside.Clock;

end Ada.Execution_Time;
