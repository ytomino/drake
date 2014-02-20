pragma License (Unrestricted);
--  extended unit
with Ada.References.Strings;
with Ada.Strings.Generic_Unbounded;
package Ada.Strings.Unbounded_Strings is
   new Generic_Unbounded (
      Character,
      String,
      References.Strings.Slicing);
pragma Preelaborate (Ada.Strings.Unbounded_Strings);
