pragma License (Unrestricted);
--  extended package
with Ada.Strings.Generic_Unbounded;
package Ada.Strings.Unbounded_Strings is
   new Generic_Unbounded (Character, String);
pragma Preelaborate (Ada.Strings.Unbounded_Strings);
