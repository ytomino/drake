with System.Native_Time;
with C.sys.resource;
package body Ada.Execution_Time.Inside is
   pragma Suppress (All_Checks);
   use type C.signed_int;

   function Clock return CPU_Time is
      rusage : aliased C.sys.resource.struct_rusage;
   begin
      if C.sys.resource.getrusage (
         C.sys.resource.RUSAGE_SELF,
         rusage'Access) < 0
      then
         raise Program_Error; -- ???
      else
         return To_Time_Span (
            System.Native_Time.To_Duration (rusage.ru_utime));
      end if;
   end Clock;

end Ada.Execution_Time.Inside;
