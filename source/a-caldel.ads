pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Native_Time;
package Ada.Calendar.Delays is

   --  required for delay statement by compiler (a-caldel.ads)
   --  the error is "entity "Ada.Calendar.Delays.Ca_Delay_For" not defined",
   --  but "Delay_For" is required in fact
   procedure Delay_For (D : Duration)
      renames System.Native_Time.Delay_For;

   --  required for delay until statement by compiler (a-caldel.ads)
   --  the error is "entity "Ada.Calendar.Delays.Ca_Delay_Until" not defined",
   --  but "Delay_Until" is required in fact
   procedure Delay_Until (T : Time);
   pragma Inline (Delay_Until);

private

   procedure Delay_Until_Body is
      new System.Native_Time.Generic_Delay_Until (Time);

   procedure Delay_Until (T : Time)
      renames Delay_Until_Body;

end Ada.Calendar.Delays;
