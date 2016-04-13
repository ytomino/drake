with C.winbase;
with C.winnt;
package body System.Native_Time is

   --  implementation

   function To_Duration (D : C.windef.FILETIME) return Duration is
      U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 0,
         LowPart => D.dwLowDateTime,
         HighPart => D.dwHighDateTime);
   begin
      return Duration'Fixed_Value ((Nanosecond_Number (U.QuadPart)) * 100);
   end To_Duration;

   procedure Simple_Delay_For (D : Duration) is
   begin
      C.winbase.Sleep (C.windef.DWORD (D * 1_000.0));
   end Simple_Delay_For;

   procedure Delay_For (D : Duration) is
   begin
      if D >= 0.0 then
         Delay_For_Hook.all (D);
      end if;
   end Delay_For;

end System.Native_Time;
