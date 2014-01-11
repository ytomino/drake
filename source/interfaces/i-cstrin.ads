pragma License (Unrestricted);
with Interfaces.C.Char_Pointers;
with Interfaces.C.Generic_Strings;
package Interfaces.C.Strings is new Generic_Strings (
   Character_Type => Character,
   String_Type => String,
   Element => char,
   Element_Array => char_array,
   Pointers => Char_Pointers,
   Length => Length,
   To_C => To_C,
   To_Ada => To_Ada);
pragma Preelaborate (Interfaces.C.Strings);
