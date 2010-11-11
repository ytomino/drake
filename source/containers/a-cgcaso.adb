with Ada.Containers.Inside.Array_Sorting;
procedure Ada.Containers.Generic_Constrained_Array_Sort (
   Container : in out Array_Type)
is
   function LT (Left, Right : Integer) return Boolean;
   function LT (Left, Right : Integer) return Boolean is
   begin
      return Container (Index_Type'Val (Left)) <
             Container (Index_Type'Val (Right));
   end LT;
   procedure Swap (Left, Right : Integer);
   procedure Swap (Left, Right : Integer) is
      Temp : constant Element_Type := Container (Index_Type'Val (Left));
   begin
      Container (Index_Type'Val (Left)) := Container (Index_Type'Val (Right));
      Container (Index_Type'Val (Right)) := Temp;
   end Swap;
begin
   Inside.Array_Sorting.In_Place_Merge_Sort (Index_Type'Pos (Container'First),
                                           Index_Type'Pos (Container'Last),
                                           LT => LT'Access,
                                           Swap => Swap'Access);
end Ada.Containers.Generic_Constrained_Array_Sort;
