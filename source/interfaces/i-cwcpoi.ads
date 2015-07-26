pragma License (Unrestricted);
--  extended unit
with Interfaces.C.Pointers;
package Interfaces.C.WChar_Pointers is
   new Pointers (
      Index => size_t,
      Element => wchar_t,
      Element_Array => wchar_array,
      Default_Terminator => wchar_t'Val (0));
--  The instance of Interfaces.C.Pointers for wchar_t.
pragma Pure (Interfaces.C.WChar_Pointers);
