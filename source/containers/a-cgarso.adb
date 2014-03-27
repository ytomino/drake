with Ada.Containers.Array_Sorting;
with System;
procedure Ada.Containers.Generic_Array_Sort (Container : in out Array_Type) is
   type Context_Type is limited record
      Container : not null access Array_Type;
   end record;
   pragma Suppress_Initialization (Context_Type);
   function LT (Left, Right : Integer; Params : System.Address)
      return Boolean;
   function LT (Left, Right : Integer; Params : System.Address)
      return Boolean
   is
      Context : Context_Type;
      for Context'Address use Params;
   begin
      return Context.Container (Index_Type'Val (Left)) <
         Context.Container (Index_Type'Val (Right));
   end LT;
   procedure Swap (Left, Right : Integer; Params : System.Address);
   procedure Swap (Left, Right : Integer; Params : System.Address) is
      Context : Context_Type;
      for Context'Address use Params;
      Temp : constant Element_Type :=
         Context.Container (Index_Type'Val (Left));
   begin
      Context.Container (Index_Type'Val (Left)) :=
         Context.Container (Index_Type'Val (Right));
      Context.Container (Index_Type'Val (Right)) := Temp;
   end Swap;
   Context : Context_Type := (Container => Container'Unrestricted_Access);
begin
   Array_Sorting.In_Place_Merge_Sort (
      Index_Type'Pos (Container'First),
      Index_Type'Pos (Container'Last),
      Context'Address,
      LT => LT'Access,
      Swap => Swap'Access);
end Ada.Containers.Generic_Array_Sort;
