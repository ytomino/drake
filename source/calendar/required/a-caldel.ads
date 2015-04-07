pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Native_Time;
private with System.Native_Calendar;
private with Ada.Unchecked_Conversion;
private package Ada.Calendar.Delays is

   --  required for delay statement by compiler (a-caldel.ads)
   procedure Delay_For (D : Duration);

   --  required for delay until statement by compiler (a-caldel.ads)
   procedure Delay_Until (T : Time);

   pragma Inline (Delay_For); -- renamed
   pragma Inline (Delay_Until); -- renamed

   --  required for select or delay by compiler (a-caldel.ads)
   function To_Duration (T : Time) return Duration;
   pragma Inline (To_Duration); -- renamed

private

   --  [gcc-4.8/4.9] Is_RTE returns False if it is renamed in the visible part.

   procedure Delay_For (D : Duration)
      renames System.Native_Time.Delay_For;

   --  [gcc-4.8/4.9] compiler could not resolve the private type Time
   --    if these instantiations are in the visible part.

   procedure Delay_Until_Body is
      new System.Native_Calendar.Generic_Delay_Until (Time);

   procedure Delay_Until (T : Time)
      renames Delay_Until_Body;

   function To_Duration_Body is new Unchecked_Conversion (Time, Duration);

   function To_Duration (T : Time) return Duration
      renames To_Duration_Body;

end Ada.Calendar.Delays;
