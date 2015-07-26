pragma License (Unrestricted);
--  extended unit
with Interfaces.C.Generic_Strings;
with Interfaces.C.WChar_Pointers;
package Interfaces.C.Wide_WStrings is
   new Generic_Strings (
      Character_Type => Wide_Character,
      String_Type => Wide_String,
      Element => wchar_t,
      Element_Array => wchar_array,
      Pointers => WChar_Pointers,
      To_C => To_wchar_array,
      To_Ada => To_Wide_String);
--  Wide_String and wchar_t version of Interfaces.C.Strings.
pragma Preelaborate (Interfaces.C.Wide_WStrings);
