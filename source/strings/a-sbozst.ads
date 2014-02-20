pragma License (Unrestricted);
--  extended unit
with Ada.References.Wide_Wide_Strings;
with Ada.Strings.Generic_Bounded;
package Ada.Strings.Bounded_Wide_Wide_Strings is
   new Generic_Bounded (
      Wide_Wide_Character,
      Wide_Wide_String,
      References.Wide_Wide_Strings.Slicing);
pragma Preelaborate (Ada.Strings.Bounded_Wide_Wide_Strings);
