with System.Native_Time;
package body System.Native_Real_Time is

   function To_uint64_t (D : Duration) return C.stdint.uint64_t;

   pragma Pure_Function (To_uint64_t);

   function To_uint64_t (D : Duration) return C.stdint.uint64_t is
   begin
      return C.stdint.uint64_t'Integer_Value (D);
   end To_uint64_t;

   --  implementation

   function To_Duration (D : Native_Time) return Duration is
   begin
      return Duration'Fixed_Value (D);
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

   procedure Generic_Delay_Until (T : Ada_Time) is
   begin
      Delay_Until (To_uint64_t (Duration (T)));
   end Generic_Delay_Until;

end System.Native_Real_Time;
