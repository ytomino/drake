pragma License (Unrestricted);
--  extended unit
with Ada.Environment_Encoding.Generic_Strings;
package Ada.Environment_Encoding.Wide_Strings is
   new Generic_Strings (
      Wide_Character,
      Wide_String);
--  Encoding / decoding between Wide_String and various encodings.
pragma Preelaborate (Ada.Environment_Encoding.Wide_Strings);
