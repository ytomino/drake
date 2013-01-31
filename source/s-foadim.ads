pragma License (Unrestricted);
--  runtime unit
procedure System.Formatting.Address_Image (
   To : out String; -- To'Length >= (Standard'Address_Size + 3) / 4
   Last : out Natural;
   Item : Address;
   Set : Type_Set := Upper_Case);
pragma Pure (System.Formatting.Address_Image);
