with System.Native_Time;
with C.winbase;
with C.windef;
package body System.Native_Real_Time is
   use type C.windef.WINBOOL;
   use type C.winnt.LONGLONG;

   subtype Positive_LONGLONG is
      C.winnt.LONGLONG range 1 .. C.winnt.LONGLONG'Last;

   Performance_Counter_Enabled : Boolean;
   Frequency : Positive_LONGLONG;

   procedure Initialize;
   procedure Initialize is
      Freq : aliased C.winnt.LARGE_INTEGER;
   begin
      Performance_Counter_Enabled :=
         C.winbase.QueryPerformanceFrequency (Freq'Access) /= C.windef.FALSE;
      Frequency := Freq.QuadPart;
   end Initialize;

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
   begin
      return C.winnt.LONGLONG (
         System.Native_Time.Nanosecond_Number'Integer_Value (T));
   end To_Native_Time;

   function To_Duration (T : Native_Time) return Duration is
   begin
      return Duration'Fixed_Value (System.Native_Time.Nanosecond_Number (T));
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
               return Count.QuadPart * 1_000_000_000 / Frequency;
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
   Initialize;
end System.Native_Real_Time;
