pragma License (Unrestricted);
--  extended package
with Ada.Strings.Generic_Unbounded;
package Ada.Strings.Unbounded_Wide_Wide_Strings is
   new Generic_Unbounded (Wide_Wide_Character, Wide_Wide_String);
pragma Preelaborate (Ada.Strings.Unbounded_Wide_Wide_Strings);
