pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Functions;
with Ada.Strings.Generic_Bounded.Generic_Functions;
package Ada.Strings.Bounded_Strings.Functions is
   new Generic_Functions (Strings.Functions);
pragma Preelaborate (Ada.Strings.Bounded_Strings.Functions);
