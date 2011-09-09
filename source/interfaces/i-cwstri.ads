pragma License (Unrestricted);
--  extended unit
with Interfaces.C.Generic_Strings;
with Interfaces.C.WChar_Pointers;
package Interfaces.C.WStrings is new Generic_Strings (
   Character_Type => wchar_Character,
   String_Type => wchar_String,
   Element => wchar_t,
   Element_Array => wchar_array,
   Pointers => WChar_Pointers);
pragma Preelaborate (Interfaces.C.WStrings);
