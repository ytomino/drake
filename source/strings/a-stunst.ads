pragma License (Unrestricted);
--  extended unit
with Ada.References.Strings;
with Ada.Streams.Block_Transmission.Strings;
with Ada.Strings.Generic_Unbounded;
package Ada.Strings.Unbounded_Strings is
   new Generic_Unbounded (
      Character,
      String,
      Streams.Block_Transmission.Strings.Read,
      Streams.Block_Transmission.Strings.Write,
      References.Strings.Slicing);
pragma Preelaborate (Ada.Strings.Unbounded_Strings);
