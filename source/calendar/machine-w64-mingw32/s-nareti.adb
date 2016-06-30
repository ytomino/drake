with System.Native_Time;
with C.winbase;
with C.windef;
package body System.Native_Real_Time is
   use type C.windef.WINBOOL;
   use type C.winnt.LONGLONG;

   Performance_Counter_Enabled : Boolean;
   Frequency : aliased C.winnt.LARGE_INTEGER;

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
   begin
      return (
         Unchecked_Tag => 255, -- any value in others
         QuadPart => C.winnt.LONGLONG (
            System.Native_Time.Nanosecond_Number'Integer_Value (T)));
   end To_Native_Time;

   function To_Duration (T : Native_Time) return Duration is
   begin
      return Duration'Fixed_Value (
         System.Native_Time.Nanosecond_Number (T.QuadPart));
   end To_Duration;

   function Clock return Native_Time is
   begin
      if Performance_Counter_Enabled then
         declare
            Count : aliased C.winnt.LARGE_INTEGER;
         begin
            if C.winbase.QueryPerformanceCounter (Count'Access) =
               C.windef.FALSE
            then
               raise Program_Error; -- ???
            else
               return (
                  Unchecked_Tag => 255, -- any value in others
                  QuadPart =>
                     Count.QuadPart * 1_000_000_000 / Frequency.QuadPart);
            end if;
         end;
      else
         raise Program_Error; -- ???
      end if;
   end Clock;

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

begin
   Performance_Counter_Enabled :=
      C.winbase.QueryPerformanceFrequency (Frequency'Access) /= C.windef.FALSE;
end System.Native_Real_Time;
