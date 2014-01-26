pragma License (Unrestricted);
--  runtime unit
procedure System.Formatting.Address_Image (
   Value : Address;
   Item : out String; -- To'Length >= (Standard'Address_Size + 3) / 4
   Last : out Natural;
   Set : Type_Set := Upper_Case);
pragma Pure (System.Formatting.Address_Image);
