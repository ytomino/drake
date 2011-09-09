pragma License (Unrestricted);
with Interfaces.C.Char_Pointers;
with Interfaces.C.Generic_Strings;
package Interfaces.C.Strings is new Generic_Strings (
   Character_Type => Interfaces.C.char,
   String_Type => String,
   Element_Array => char_array,
   Pointers => Char_Pointers);
pragma Pure (Interfaces.C.Strings);
