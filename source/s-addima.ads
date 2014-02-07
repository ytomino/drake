pragma License (Unrestricted);
--  implementation unit required by compiler for nested tagged types
with System.Storage_Elements.Formatting;
function System.Address_Image (A : Address)
   return Storage_Elements.Formatting.Address_String;
pragma Pure (System.Address_Image);
pragma Inline (System.Address_Image);
