package body System.Native_Real_Time is
   pragma Suppress (All_Checks);
   use type C.signed_int;

   function Clock return Native_Time is
      Result : aliased C.time.struct_timespec;
   begin
      if C.time.clock_gettime (C.time.CLOCK_MONOTONIC, Result'Access) < 0 then
         raise Program_Error; -- ???
      end if;
      return Result;
   end Clock;

   procedure Simple_Delay_Until (T : Native_Time) is
      Timeout_T : constant Duration := To_Duration (T);
      Current_T : constant Duration := To_Duration (Clock);
      D : Duration;
   begin
      if Timeout_T > Current_T then
         D := Timeout_T - Current_T;
      else
         D := 0.0; -- always calling Delay_For for abort checking
      end if;
      System.Native_Time.Delay_For (D); -- unimplemented in tasking
   end Simple_Delay_Until;

   procedure Delay_Until (T : Native_Time) is
   begin
      Delay_Until_Hook.all (T);
   end Delay_Until;

   procedure Generic_Delay_Until (T : Ada_Time) is
   begin
      Delay_Until (System.Native_Time.To_timespec (Duration (T)));
   end Generic_Delay_Until;

end System.Native_Real_Time;
