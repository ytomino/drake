pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Wide_Wide_Functions;
package Ada.Strings.Unbounded_Wide_Wide_Strings.Functions is
   new Generic_Functions (Wide_Wide_Functions);
pragma Preelaborate (Ada.Strings.Unbounded_Wide_Wide_Strings.Functions);
