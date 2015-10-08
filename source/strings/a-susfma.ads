pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Functions.Maps;
package Ada.Strings.Unbounded_Strings.Functions.Maps is
   new Generic_Maps (Strings.Functions.Maps);
pragma Preelaborate (Ada.Strings.Unbounded_Strings.Functions.Maps);
