package body System.Finalization_Root is
   pragma Suppress (All_Checks);

   function "=" (Left, Right : Root_Controlled) return Boolean is
      pragma Unreferenced (Left, Right);
   begin
      return True;
   end "=";

end System.Finalization_Root;
