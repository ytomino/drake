pragma License (Unrestricted);
--  implementation unit required by compiler
private with System.Native_Real_Time;
private package Ada.Real_Time.Delays is

   --  required for delay until statement by compiler (a-retide.ads)
   procedure Delay_Until (T : Time);
   pragma Inline (Delay_Until); -- renamed

   --  required by compiler ??? (a-retide.ads)
--  function To_Duration (T : Time) return Duration;

private

   --  [gcc-4.8/4.9] compiler could not resolve the private type Time
   --    if this instantiation is in the visible part.

   procedure Delay_Until_Body is
      new System.Native_Real_Time.Generic_Delay_Until (Time);

   procedure Delay_Until (T : Time)
      renames Delay_Until_Body;

end Ada.Real_Time.Delays;
