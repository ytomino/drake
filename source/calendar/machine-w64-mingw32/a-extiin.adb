with System.Native_Time;
with C.windef;
with C.winbase;
package body Ada.Execution_Time.Inside is
   pragma Suppress (All_Checks);
   use type C.windef.WINBOOL;

   --  implementation

   function Clock return CPU_Time is
      CreationTime : aliased C.windef.FILETIME;
      ExitTime : aliased C.windef.FILETIME;
      KernelTime : aliased C.windef.FILETIME;
      UserTime : aliased C.windef.FILETIME;
   begin
      if C.winbase.GetProcessTimes (
         C.winbase.GetCurrentProcess,
         CreationTime'Access,
         ExitTime'Access,
         KernelTime'Access,
         UserTime'Access) = 0
      then
         raise Program_Error; -- ???
      else
         return To_Time_Span (System.Native_Time.To_Duration (UserTime));
      end if;
   end Clock;

end Ada.Execution_Time.Inside;
