pragma License (Unrestricted);
--  extended unit
with Interfaces.C.Pointers;
package Interfaces.C.Char_Pointers is new Pointers (
   Index => size_t,
   Element => char,
   Element_Array => char_array,
   Default_Terminator => char'Val (0));
pragma Pure (Interfaces.C.Char_Pointers);
