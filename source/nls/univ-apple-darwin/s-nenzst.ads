pragma License (Unrestricted);
--  extended unit
with System.Native_Encoding.Generic_Strings;
package System.Native_Encoding.Wide_Wide_Strings is
   new Generic_Strings (
      Wide_Wide_Character,
      Wide_Wide_String);
--  Encoding / decoding between Wide_Wide_String and various encodings.
pragma Preelaborate (System.Native_Encoding.Wide_Wide_Strings);
