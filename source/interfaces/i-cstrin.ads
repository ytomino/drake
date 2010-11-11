pragma License (Unrestricted);
with Interfaces.C.Zero_Terminated_Strings;
with Interfaces.C.Char_Pointers;
package Interfaces.C.Strings is new Interfaces.C.Zero_Terminated_Strings (
   Character_Type => Interfaces.C.char,
   String_Type => String,
   Element_Array => char_array,
   Pointers => Char_Pointers);
pragma Pure (Interfaces.C.Strings);
