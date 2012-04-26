with Ada.Containers.Inside.Array_Sorting;
with System;
procedure Ada.Containers.Generic_Sort (First, Last : Index_Type'Base) is
   function LT (Left, Right : Integer; Params : System.Address)
      return Boolean;
   function LT (Left, Right : Integer; Params : System.Address)
      return Boolean
   is
      pragma Unreferenced (Params);
   begin
      return Before (Index_Type'Val (Left), Index_Type'Val (Right));
   end LT;
   procedure Swap (Left, Right : Integer; Params : System.Address);
   procedure Swap (Left, Right : Integer; Params : System.Address) is
      pragma Unreferenced (Params);
   begin
      Swap (Index_Type'Val (Left), Index_Type'Val (Right));
   end Swap;
begin
   Inside.Array_Sorting.In_Place_Merge_Sort (
      Index_Type'Pos (First),
      Index_Type'Pos (Last),
      System.Null_Address,
      LT => LT'Access,
      Swap => Swap'Access);
end Ada.Containers.Generic_Sort;
