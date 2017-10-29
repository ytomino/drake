pragma License (Unrestricted);
--  implementation unit required by compiler
private with System.Native_Time;
private package Ada.Calendar.Delays is

   --  required for delay statement by compiler (a-caldel.ads)
   procedure Delay_For (D : Duration);
   pragma Inline (Delay_For); -- renamed

   --  required for delay until statement by compiler (a-caldel.ads)
   procedure Delay_Until (T : Time);

   --  required by compiler (a-caldel.ads)
   --  for select or delay expanded to Timed_Task_Entry_Call,
   --    Timed_Protected_Entry_Call, or Timed_Selective_Wait (exp_ch9.adb)
   function To_Duration (T : Time) return Duration;
   pragma Inline (To_Duration);

private

   --  [gcc-4.8/4.9] Is_RTE returns False if it is renamed in the visible part.

   procedure Delay_For (D : Duration)
      renames System.Native_Time.Delay_For;

end Ada.Calendar.Delays;
