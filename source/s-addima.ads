pragma License (Unrestricted);
--  implementation unit required by compiler for nested tagged types
with System.Storage_Elements;
function System.Address_Image (A : Address)
   return Storage_Elements.Address_Image;
pragma Pure (System.Address_Image);
