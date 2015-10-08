pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Wide_Functions.Maps;
package Ada.Strings.Bounded_Wide_Strings.Functions.Maps is
   new Generic_Maps (Wide_Functions.Maps);
pragma Preelaborate (Ada.Strings.Bounded_Wide_Strings.Functions.Maps);
