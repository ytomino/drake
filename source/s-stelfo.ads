pragma License (Unrestricted);
--  extended unit
package System.Storage_Elements.Formatting is
   --  The formatting functions Image and Value for System.Address.
   pragma Pure;

   subtype Address_String is String (1 .. (Standard'Address_Size + 3) / 4);

   function Image (Value : Address) return Address_String;
   function Value (Value : Address_String) return Address;

end System.Storage_Elements.Formatting;
