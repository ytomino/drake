pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Functions;
package Ada.Strings.Unbounded_Strings.Functions is
   new Generic_Functions (Strings.Functions);
pragma Preelaborate (Ada.Strings.Unbounded_Strings.Functions);
