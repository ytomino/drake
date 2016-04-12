pragma License (Unrestricted);
--  implementation unit required by compiler
private package Ada.Real_Time.Delays is

   --  required for delay until statement by compiler (a-retide.ads)
   procedure Delay_Until (T : Time);

   --  required by compiler ??? (a-retide.ads)
--  function To_Duration (T : Time) return Duration;

end Ada.Real_Time.Delays;
