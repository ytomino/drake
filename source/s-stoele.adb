package body System.Storage_Elements is
   pragma Suppress (All_Checks);

   function "-" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Left - Address (Right);
   end "-";

   function "-" (Left, Right : Address) return Storage_Offset is
   begin
      return Storage_Offset (Left) - Storage_Offset (Right);
   end "-";

   function "mod" (Left : Address; Right : Storage_Offset)
      return Storage_Offset is
   begin
      return Storage_Offset (Left mod Address (Right)); --  unsigned mod
   end "mod";

end System.Storage_Elements;
