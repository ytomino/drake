with System.Formatting.Address_Image;
package body System.Storage_Elements.Formatting is
   pragma Suppress (All_Checks);

   --  implementation

   function Image (Value : Address) return Address_String is
      Last : Natural;
   begin
      return Result : Address_String do
         System.Formatting.Address_Image (
            Value,
            Result,
            Last,
            Set => System.Formatting.Upper_Case);
      end return;
   end Image;

   function Value (Value : Address_String) return Address is
      Use_Longest : constant Boolean :=
         Standard'Address_Size > System.Formatting.Unsigned'Size;
      Result : Address;
      Last : Natural;
      Error : Boolean;
   begin
      if Use_Longest then
         System.Formatting.Value (
            Value,
            Last,
            System.Formatting.Longest_Unsigned (Result),
            Base => 16,
            Error => Error);
      else
         System.Formatting.Value (
            Value,
            Last,
            System.Formatting.Unsigned (Result),
            Base => 16,
            Error => Error);
      end if;
      if Last /= Value'Last then
         raise Constraint_Error;
      end if;
      return Result;
   end Value;

end System.Storage_Elements.Formatting;
