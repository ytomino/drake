with System.Formatting;
function System.Address_Image (A : Address)
   return Storage_Elements.Address_Image
is
   Use_Longest : constant Boolean :=
      Standard'Address_Size > Formatting.Unsigned'Size;
   Last : Natural;
   Error : Boolean;
begin
   return Result : Storage_Elements.Address_Image do
      if Use_Longest then
         Formatting.Image (
            Formatting.Longest_Unsigned (A),
            Result,
            Last,
            Base => 16,
            Width => Storage_Elements.Address_Image'Length,
            Error => Error);
      else
         Formatting.Image (
            Formatting.Unsigned (A),
            Result,
            Last,
            Base => 16,
            Width => Storage_Elements.Address_Image'Length,
            Error => Error);
      end if;
      pragma Assert (not Error and then Last = Result'Last);
   end return;
end System.Address_Image;
