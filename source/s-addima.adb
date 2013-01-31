with System.Formatting.Address_Image;
function System.Address_Image (A : Address)
   return Storage_Elements.Address_Image
is
   pragma Suppress (All_Checks);
   Last : Natural;
begin
   return Result : Storage_Elements.Address_Image do
      Formatting.Address_Image (
         Result,
         Last,
         A,
         Casing => Formatting.Upper);
   end return;
end System.Address_Image;
