with Ada.Unchecked_Conversion;
with System.Native_Time;
with C.windef;
with C.winbase;
with C.winnt;
package body Ada.Execution_Time.Inside is
   pragma Suppress (All_Checks);
   use type System.Native_Time.Nanosecond_Number;
   use type C.windef.WINBOOL;

   --  for shorthand
   subtype Nanosecond_Number is System.Native_Time.Nanosecond_Number;

   Performance_Counter_Enabled : Boolean;
   Frequency : aliased C.winnt.LARGE_INTEGER;

   --  implementation

   function Clock return CPU_Time is
      function Cast is new Unchecked_Conversion (Duration, CPU_Time);
   begin
      if Performance_Counter_Enabled then
         declare
            Count : aliased C.winnt.LARGE_INTEGER;
         begin
            if C.winbase.QueryPerformanceCounter (Count'Access) = 0 then
               raise Program_Error; -- ???
            else
               return Cast (Duration'Fixed_Value (
                  Nanosecond_Number (Count.QuadPart)
                     * 1000_000_000
                     / Nanosecond_Number (Frequency.QuadPart)));
            end if;
         end;
      else
         return CPU_Time (Real_Time.Clock); -- fallback
      end if;
   end Clock;

begin
   Performance_Counter_Enabled :=
      C.winbase.QueryPerformanceFrequency (Frequency'Access) /= 0;
end Ada.Execution_Time.Inside;
