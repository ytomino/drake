pragma License (Unrestricted);
--  implementation unit required by compiler
private with Ada.Unchecked_Conversion;
private with System.Native_Time;
private package Ada.Calendar.Delays is

   --  required for delay statement by compiler (a-caldel.ads)
   --  the error is "entity "Ada.Calendar.Delays.Ca_Delay_For" not defined",
   --  but "Delay_For" is required in fact
   --  note, Is_RTE returns False if it is declared with renaming directly
   procedure Delay_For (D : Duration);
   pragma Inline (Delay_For);

   --  required for delay until statement by compiler (a-caldel.ads)
   --  the error is "entity "Ada.Calendar.Delays.Ca_Delay_Until" not defined",
   --  but "Delay_Until" is required in fact
   procedure Delay_Until (T : Time);
   pragma Inline (Delay_Until);

   --  required for select or delay by compiler (a-caldel.ads)
   function To_Duration (T : Time) return Duration;
   pragma Inline (To_Duration);

private

   procedure Delay_For (D : Duration)
      renames System.Native_Time.Delay_For;

   procedure Delay_Until_Body is
      new System.Native_Time.Generic_Delay_Until (Time);

   procedure Delay_Until (T : Time)
      renames Delay_Until_Body;

   function To_Duration_Body is new Unchecked_Conversion (Time, Duration);

   function To_Duration (T : Time) return Duration
      renames To_Duration_Body;

end Ada.Calendar.Delays;
