pragma License (Unrestricted);
--  extended unit
with Ada.Environment_Encoding.Generic_Strings;
package Ada.Environment_Encoding.Strings is
   new Generic_Strings (
      Character,
      String);
--  Encoding / decoding between String and various encodings.
pragma Preelaborate (Ada.Environment_Encoding.Strings);
