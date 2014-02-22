pragma License (Unrestricted);
--  extended unit
with Ada.References.Wide_Strings;
with Ada.Streams.Block_Transmission.Wide_Strings;
with Ada.Strings.Generic_Bounded;
package Ada.Strings.Bounded_Wide_Strings is
   new Generic_Bounded (
      Wide_Character,
      Wide_String,
      Streams.Block_Transmission.Wide_Strings.Read,
      Streams.Block_Transmission.Wide_Strings.Write,
      References.Wide_Strings.Slicing);
pragma Preelaborate (Ada.Strings.Bounded_Wide_Strings);
