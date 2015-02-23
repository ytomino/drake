package body System.Native_Time is
   pragma Suppress (All_Checks);
   use type C.signed_int;
   use type C.signed_long;

   Diff : constant := 5680281600.0;
   --  seconds from 1970-01-01 (0 of POSIX time) to 2150-01-01 (0 of Ada time)

   function To_timespec (X : C.sys.time.struct_timeval)
      return C.time.struct_timespec;
   function To_timespec (X : C.sys.time.struct_timeval)
      return C.time.struct_timespec is
   begin
      return (
         tv_sec => X.tv_sec,
         tv_nsec => C.signed_long (X.tv_usec) * 1000);
   end To_timespec;

   function To_Duration (D : C.sys.types.time_t) return Duration;
   function To_Duration (D : C.sys.types.time_t) return Duration is
   begin
      return Duration'Fixed_Value (
         (Nanosecond_Number (D)) * 1000_000_000);
   end To_Duration;

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
   begin
      return To_Native_Duration (T + Diff);
   end To_Native_Time;

   function To_Time (T : Native_Time) return Duration is
   begin
      return To_Duration (T) - Diff;
   end To_Time;

   function To_Time (T : C.sys.types.time_t) return Duration is
   begin
      return To_Duration (T) - Diff;
   end To_Time;

   function To_Native_Duration (D : Duration) return Native_Time is
      Nanosecond : constant Nanosecond_Number :=
         Nanosecond_Number'Integer_Value (D);
      Sub_Second : constant Nanosecond_Number := Nanosecond mod 1000_000_000;
   begin
      return (
         tv_sec =>
            C.sys.types.time_t ((Nanosecond - Sub_Second) / 1000_000_000),
         tv_nsec =>
            C.signed_long (Sub_Second));
   end To_Native_Duration;

   function To_Duration (D : Native_Time) return Duration is
   begin
      return Duration'Fixed_Value (
         Nanosecond_Number'Integer_Value (To_Duration (D.tv_sec))
         + Nanosecond_Number (D.tv_nsec));
   end To_Duration;

   function To_Duration (D : C.sys.time.struct_timeval) return Duration is
   begin
      return To_Duration (To_timespec (D));
   end To_Duration;

   function Clock return Native_Time is
      Result : aliased C.sys.time.struct_timeval;
      R : C.signed_int;
   begin
      R := C.sys.time.gettimeofday (Result'Access, null);
      if R < 0 then
         raise Program_Error; -- ???
      end if;
      return To_timespec (Result);
   end Clock;

   procedure Simple_Delay_For (D : Duration) is
      Req_T : aliased C.time.struct_timespec := To_Native_Duration (D);
      Rem_T : aliased C.time.struct_timespec;
      R : C.signed_int;
   begin
      loop
         R := C.time.nanosleep (Req_T'Access, Rem_T'Access);
         exit when R = 0;
         Req_T := Rem_T;
      end loop;
   end Simple_Delay_For;

   procedure Delay_For (D : Duration) is
   begin
      if D >= 0.0 then
         Delay_For_Hook.all (D);
      end if;
   end Delay_For;

   procedure Simple_Delay_Until (T : Native_Time) is
      Timeout_T : constant Duration := To_Duration (T);
      Current_T : constant Duration := To_Duration (Clock);
   begin
      if Timeout_T > Current_T then
         Simple_Delay_For (Timeout_T - Current_T);
      end if;
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
