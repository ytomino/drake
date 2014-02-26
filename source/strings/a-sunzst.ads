pragma License (Unrestricted);
--  extended unit
with Ada.References.Wide_Wide_Strings;
with Ada.Streams.Block_Transmission.Wide_Wide_Strings;
with Ada.Strings.Generic_Unbounded;
package Ada.Strings.Unbounded_Wide_Wide_Strings is
   new Generic_Unbounded (
      Wide_Wide_Character,
      Wide_Wide_String,
      Streams.Block_Transmission.Wide_Wide_Strings.Read,
      Streams.Block_Transmission.Wide_Wide_Strings.Write,
      References.Wide_Wide_Strings.Slicing);
pragma Preelaborate (Ada.Strings.Unbounded_Wide_Wide_Strings);
