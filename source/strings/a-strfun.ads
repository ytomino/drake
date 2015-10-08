pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Generic_Functions;
package Ada.Strings.Functions is
   new Generic_Functions (
      Character_Type => Character,
      String_Type => String,
      Space => Space);
pragma Preelaborate (Ada.Strings.Functions);
