pragma License (Unrestricted);
--  extended unit
with Interfaces.C.Zero_Terminated_Strings;
with Interfaces.C.WChar_Pointers;
package Interfaces.C.WStrings is new Interfaces.C.Zero_Terminated_Strings (
   Character_Type => Interfaces.C.wchar_t,
   String_Type => Wide_Wide_String,
   Element_Array => wchar_array,
   Pointers => WChar_Pointers);
pragma Pure (Interfaces.C.WStrings);
