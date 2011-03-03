pragma License (Unrestricted);
--  implementation package required by compiler
private with Ada.Calendar.Inside;
package Ada.Calendar.Delays is

   --  required for delay statement by compiler (a-caldel.ads)
   --  the error is "entity "Ada.Calendar.Delays.Ca_Delay_For" not defined",
   --  but "Delay_For" is required in fact
   procedure Delay_For (D : Duration);
   pragma Inline_Always (Delay_For);

   --  required for delay until statement by compiler (a-caldel.ads)
   --  the error is "entity "Ada.Calendar.Delays.Ca_Delay_Until" not defined",
   --  but "Delay_Until" is required in fact
   procedure Delay_Until (T : Time);
   pragma Inline_Always (Delay_Until);

private

   procedure Delay_For (D : Duration)
      renames Inside.Delay_For;

   procedure Delay_Until (T : Time)
      renames Inside.Delay_Until;

end Ada.Calendar.Delays;
