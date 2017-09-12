pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with C.sys.time; -- struct timeval
with C.sys.types; -- time_t
with C.time; -- struct timespec
package System.Native_Time is
   pragma Preelaborate;

   --  representation

   type Nanosecond_Number is
      range -(2 ** (Duration'Size - 1)) .. 2 ** (Duration'Size - 1) - 1;
   for Nanosecond_Number'Size use Duration'Size;

   --  convert time span

   function To_timespec (D : C.sys.time.struct_timeval)
      return C.time.struct_timespec;
   function To_timespec (D : Duration) return C.time.struct_timespec;
   function To_Duration (D : C.time.struct_timespec) return Duration;
   function To_Duration (D : C.sys.time.struct_timeval) return Duration;
   function To_Duration (D : C.sys.types.time_t) return Duration;

   pragma Pure_Function (To_timespec);
   pragma Pure_Function (To_Duration);

   --  for delay

   procedure Simple_Delay_For (D : Duration);

   type Delay_For_Handler is access procedure (D : Duration);
   pragma Favor_Top_Level (Delay_For_Handler);

   --  equivalent to Timed_Delay (s-soflin.ads)
   Delay_For_Hook : not null Delay_For_Handler := Simple_Delay_For'Access;

   procedure Delay_For (D : Duration);

end System.Native_Time;
