pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
package System.Native_Execution_Time is

   subtype CPU_Time is Duration;

   function Clock return CPU_Time;

end System.Native_Execution_Time;
