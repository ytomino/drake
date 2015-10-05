pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Wide_Functions;
package Ada.Strings.Bounded_Wide_Strings.Functions is
   new Generic_Functions (Wide_Functions);
pragma Preelaborate (Ada.Strings.Bounded_Wide_Strings.Functions);
