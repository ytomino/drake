pragma License (Unrestricted);
with Ada.Strings.Generic_Fixed;
--  with Ada.Strings.Wide_Maps;
package Ada.Strings.Wide_Fixed is new Generic_Fixed (
   Character_Type => Wide_Character,
   String_Type => Wide_String,
   Space => Wide_Space);
pragma Preelaborate (Ada.Strings.Wide_Fixed);
