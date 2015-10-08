pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Generic_Functions;
package Ada.Strings.Wide_Wide_Functions is
   new Generic_Functions (
      Character_Type => Wide_Wide_Character,
      String_Type => Wide_Wide_String,
      Space => Wide_Wide_Space);
pragma Preelaborate (Ada.Strings.Wide_Wide_Functions);
