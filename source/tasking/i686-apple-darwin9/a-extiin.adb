with Ada.Calendar.Inside;
with Ada.Unchecked_Conversion;
with C.sys.resource;
package body Ada.Execution_Time.Inside is
   pragma Suppress (All_Checks);
   use type C.signed_int;

   function Clock return CPU_Time is
      function Cast is new Unchecked_Conversion (Calendar.Time, CPU_Time);
      rusage : aliased C.sys.resource.struct_rusage;
   begin
      if C.sys.resource.getrusage (
         C.sys.resource.RUSAGE_SELF,
         rusage'Access) < 0
      then
         raise Program_Error;
      else
         return Cast (Calendar.Inside.To_Time (rusage.ru_utime));
      end if;
   end Clock;

end Ada.Execution_Time.Inside;
