with System.Native_Time;
with C.sys.resource;
with C.sys.time;
package body System.Native_Execution_Time is
   use type C.signed_int;

   function To_Duration (D : C.sys.time.struct_timeval) return Duration;
   function To_Duration (D : C.sys.time.struct_timeval) return Duration is
   begin
      return Native_Time.To_Duration (Native_Time.To_timespec (D));
   end To_Duration;

   --  implementation

   function Clock return CPU_Time is
      rusage : aliased C.sys.resource.struct_rusage;
   begin
      if C.sys.resource.getrusage (
         C.sys.resource.RUSAGE_SELF,
         rusage'Access) < 0
      then
         raise Program_Error; -- ???
      else
         return To_Duration (rusage.ru_utime);
      end if;
   end Clock;

end System.Native_Execution_Time;
