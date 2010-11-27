pragma License (Unrestricted);
--  implementation required for nested tagged types by compiler
with System.Storage_Elements;
function System.Address_Image (A : Address)
   return Storage_Elements.Address_Image;
pragma Pure (System.Address_Image);
