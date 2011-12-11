pragma License (Unrestricted);
with Ada.References.String;
with Interfaces.C.Char_Pointers;
with Interfaces.C.Generic_Strings;
package Interfaces.C.Strings is new Generic_Strings (
   Character_Type => Character,
   String_Type => String,
   Element => char,
   Element_Array => char_array,
   Pointers => Char_Pointers,
   Slicing => Ada.References.String.Slicing);
pragma Preelaborate (Interfaces.C.Strings);
