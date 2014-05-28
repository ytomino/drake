pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Native_Time;
private package Ada.Real_Time.Delays is

   --  required for delay until statement by compiler (a-retide.ads)
   --  the error message is
   --    'entity "Ada.Real_Time.Delays.Rt_Delay_Until" not available',
   --  but "Delay_Until" is required in fact.
   procedure Delay_Until (T : Time);
   pragma Inline (Delay_Until); -- renamed

   --  required by compiler ??? (a-retide.ads)
--  function To_Duration (T : Time) return Duration;

private

   procedure Delay_Until_Body is
      new System.Native_Time.Generic_Delay_Until (Time);

   procedure Delay_Until (T : Time)
      renames Delay_Until_Body;

end Ada.Real_Time.Delays;
