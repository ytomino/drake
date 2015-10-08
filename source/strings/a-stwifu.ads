pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Generic_Functions;
package Ada.Strings.Wide_Functions is
   new Generic_Functions (
      Character_Type => Wide_Character,
      String_Type => Wide_String,
      Space => Wide_Space);
pragma Preelaborate (Ada.Strings.Wide_Functions);
