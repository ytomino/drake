pragma License (Unrestricted);
with Ada.Strings.Generic_Fixed;
--  with Ada.Strings.Wide_Wide_Maps;
package Ada.Strings.Wide_Wide_Fixed is new Generic_Fixed (
   Character_Type => Wide_Wide_Character,
   String_Type => Wide_Wide_String,
   Space => Wide_Wide_Space);
pragma Preelaborate (Ada.Strings.Wide_Wide_Fixed);
