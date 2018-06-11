with System.Native_Time;
with C.winbase;
with C.windef;
package body System.Native_Execution_Time is
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
            UserTime'Access) =
         C.windef.FALSE
      then
         raise Program_Error; -- ???
      else
         return Native_Time.To_Duration (UserTime);
      end if;
   end Clock;

end System.Native_Execution_Time;
