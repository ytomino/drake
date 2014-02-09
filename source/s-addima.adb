function System.Address_Image (A : Address)
   return Storage_Elements.Formatting.Address_String
is
   pragma Suppress (All_Checks);
begin
   return Storage_Elements.Formatting.Image (A);
end System.Address_Image;
