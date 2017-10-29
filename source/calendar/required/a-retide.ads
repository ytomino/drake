pragma License (Unrestricted);
--  implementation unit required by compiler
private package Ada.Real_Time.Delays is

   --  required for delay until statement by compiler (a-retide.ads)
   procedure Delay_Until (T : Time);

   --  required by compiler (a-retide.ads)
   --  for select or delay expanded to Timed_Task_Entry_Call,
   --    Timed_Protected_Entry_Call, or Timed_Selective_Wait (exp_ch9.adb)
   --  for pragma Relative_Deadline (exp_prag.adb)
   function To_Duration (T : Time) return Duration;
   pragma Pure_Function (To_Duration);
   pragma Inline (To_Duration);

end Ada.Real_Time.Delays;
