with System.Formatting.Address;
package body System.Storage_Elements.Formatting is
   pragma Suppress (All_Checks);

   --  implementation

   function Image (Value : Address) return Address_String is
   begin
      return Result : Address_String do
         System.Formatting.Address.Image (
            Value,
            Result,
            Set => System.Formatting.Upper_Case);
      end return;
   end Image;

   function Value (Value : Address_String) return Address is
      Result : Address;
      Error : Boolean;
   begin
      System.Formatting.Address.Value (
         Value,
         Result,
         Error => Error);
      if Error then
         raise Constraint_Error;
      end if;
      return Result;
   end Value;

end System.Storage_Elements.Formatting;
