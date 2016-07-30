pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Generic_Unbounded.Generic_Functions;
with Ada.Strings.Wide_Functions;
package Ada.Strings.Unbounded_Wide_Strings.Functions is
   new Generic_Functions (Wide_Functions);
pragma Preelaborate (Ada.Strings.Unbounded_Wide_Strings.Functions);
