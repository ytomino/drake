with System.Native_Time;
package body System.Native_Real_Time is

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
   begin
      return C.stdint.uint64_t'Integer_Value (T);
   end To_Native_Time;

   function To_Duration (T : Native_Time) return Duration is
   begin
      return Duration'Fixed_Value (T);
   end To_Duration;

   procedure Delay_Until (T : Native_Time) is
      Timeout_T : constant Duration := To_Duration (T);
      Current_T : constant Duration := To_Duration (Clock);
      D : Duration;
   begin
      if Timeout_T > Current_T then
         D := Timeout_T - Current_T;
      else
         D := 0.0; -- always calling Delay_For for abort checking
      end if;
      System.Native_Time.Delay_For (D);
   end Delay_Until;

end System.Native_Real_Time;
