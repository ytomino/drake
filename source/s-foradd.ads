pragma License (Unrestricted);
--  runtime unit
package System.Formatting.Address is
   pragma Pure;

   subtype Address_String is String (1 .. (Standard'Address_Size + 3) / 4);

   procedure Image (
      Value : System.Address;
      Item : out Address_String;
      Set : Type_Set := Upper_Case);

   procedure Value (
      Item : Address_String;
      Result : out System.Address;
      Error : out Boolean);

end System.Formatting.Address;
