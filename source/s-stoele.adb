package body System.Storage_Elements is
   pragma Suppress (All_Checks);

   function "+" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return System'To_Address (
         Integer_Address (Left) + Integer_Address'Mod (Right));
   end "+";

   function "+" (Left : Storage_Offset; Right : Address) return Address is
   begin
      return System'To_Address (
         Integer_Address'Mod (Left) + Integer_Address (Right));
   end "+";

   function "-" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return System'To_Address (
         Integer_Address (Left) - Integer_Address'Mod (Right));
   end "-";

   function "-" (Left, Right : Address) return Storage_Offset is
   begin
      return Storage_Offset (Left) - Storage_Offset (Right);
   end "-";

   function "mod" (Left : Address; Right : Storage_Offset)
      return Storage_Offset is
   begin
      return Storage_Offset (Left mod Address (Right)); -- unsigned mod
   end "mod";

   function To_Address (Value : Integer_Address) return Address is
   begin
      return Address (Value);
   end To_Address;

   function To_Integer (Value : Address) return Integer_Address is
   begin
      return Integer_Address (Value);
   end To_Integer;

end System.Storage_Elements;
