pragma License (Unrestricted);
--  implementation unit specialized for Windows
package System.Native_Execution_Time is

   subtype CPU_Time is Duration;

   function Clock return CPU_Time;

end System.Native_Execution_Time;
