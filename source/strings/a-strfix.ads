pragma License (Unrestricted);
with Ada.Strings.Generic_Fixed;
--  with Ada.Strings.Maps;
package Ada.Strings.Fixed is new Generic_Fixed (
   Character_Type => Character,
   String_Type => String,
   Space => Space);
pragma Preelaborate (Ada.Strings.Fixed);
