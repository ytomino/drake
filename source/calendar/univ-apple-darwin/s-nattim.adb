with Ada.Unchecked_Conversion;
package body System.Native_Time is
   pragma Suppress (All_Checks);
   use type C.signed_long;

   type Time_Rep is range
      -(2 ** (Duration'Size - 1)) ..
      +(2 ** (Duration'Size - 1)) - 1;
   for Time_Rep'Size use Duration'Size;

   Diff : constant Time_Rep := -5680281600;
   --  seconds from 1971-01-01 (0 of POSIX time) to 2150-01-01 (0 of Ada time)

   function To_timespec (X : C.sys.time.struct_timeval)
      return C.time.struct_timespec;
   function To_timespec (X : C.sys.time.struct_timeval)
      return C.time.struct_timespec is
   begin
      return (
         tv_sec => X.tv_sec,
         tv_nsec => C.signed_long (X.tv_usec) * 1000);
   end To_timespec;

   function To_timespec_Duration (D : Duration)
      return C.time.struct_timespec;
   function To_timespec_Duration (D : Duration)
      return C.time.struct_timespec
   is
      function Cast is new Ada.Unchecked_Conversion (Duration, Time_Rep);
      Sub_Second : constant Time_Rep := Cast (D) mod 1000000000;
   begin
      return (
         tv_sec =>
            C.sys.types.time_t ((Cast (D) - Sub_Second) / 1000000000),
         tv_nsec =>
            C.signed_long (Sub_Second));
   end To_timespec_Duration;

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
      function Cast is new Ada.Unchecked_Conversion (Duration, Time_Rep);
      Sub_Second : constant Time_Rep := Cast (T) mod 1000000000;
   begin
      return (
         tv_sec =>
            C.sys.types.time_t ((Cast (T) - Sub_Second) / 1000000000 - Diff),
         tv_nsec =>
            C.signed_long (Sub_Second));
   end To_Native_Time;

   function To_Time (T : Native_Time) return Duration is
      function Cast is new Ada.Unchecked_Conversion (Duration, Time_Rep);
      function Cast is new Ada.Unchecked_Conversion (Time_Rep, Duration);
   begin
      return Cast (Cast (To_Time (T.tv_sec)) + Time_Rep (T.tv_nsec));
   end To_Time;

   function To_Time (T : C.sys.types.time_t) return Duration is
      function Cast is new Ada.Unchecked_Conversion (Time_Rep, Duration);
   begin
      return Cast ((Time_Rep (T) + Diff) * 1000000000);
   end To_Time;

   function To_Time (T : C.sys.time.struct_timeval) return Duration is
   begin
      return To_Time (To_timespec (T));
   end To_Time;

   function Clock return Native_Time is
      Result : aliased C.sys.time.struct_timeval;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.sys.time.gettimeofday (Result'Access, null);
      return To_timespec (Result);
   end Clock;

   procedure Simple_Delay_For (D : Duration) is
   begin
      if D > 0.0 then
         declare
            T : aliased C.time.struct_timespec := To_timespec_Duration (D);
            Dummy : C.signed_int;
            pragma Unreferenced (Dummy);
         begin
            Dummy := C.time.nanosleep (T'Access, null);
         end;
      end if;
   end Simple_Delay_For;

   procedure Delay_For (D : Duration) is
   begin
      Delay_For_Hook.all (D);
   end Delay_For;

   procedure Simple_Delay_Until (T : Native_Time) is
   begin
      Simple_Delay_For (To_Time (T) - To_Time (Clock));
   end Simple_Delay_Until;

   procedure Delay_Until (T : Native_Time) is
   begin
      Delay_Until_Hook.all (T);
   end Delay_Until;

   procedure Generic_Delay_Until (T : Ada_Time) is
   begin
      Delay_Until (To_Native_Time (Duration (T)));
   end Generic_Delay_Until;

end System.Native_Time;
