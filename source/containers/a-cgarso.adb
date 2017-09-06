with Ada.Containers.Array_Sorting;
with System;
procedure Ada.Containers.Generic_Array_Sort (Container : in out Array_Type) is
begin
   if Index_Type'Pos (Index_Type'First) in
         Long_Long_Integer (Integer'First) ..
         Long_Long_Integer (Integer'Last)
      and then Index_Type'Pos (Index_Type'Last) in
         Long_Long_Integer (Integer'First) ..
         Long_Long_Integer (Integer'Last)
   then
      declare
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
            Left_Index : constant Index_Type := Index_Type'Val (Left);
            Right_Index : constant Index_Type := Index_Type'Val (Right);
         begin
            return Context.Container (Left_Index) <
               Context.Container (Right_Index);
         end LT;
         procedure Swap (Left, Right : Integer; Params : System.Address);
         procedure Swap (Left, Right : Integer; Params : System.Address) is
            Context : Context_Type;
            for Context'Address use Params;
            Left_Index : constant Index_Type := Index_Type'Val (Left);
            Right_Index : constant Index_Type := Index_Type'Val (Right);
            Temp : constant Element_Type := Context.Container (Left_Index);
         begin
            Context.Container (Left_Index) := Context.Container (Right_Index);
            Context.Container (Right_Index) := Temp;
         end Swap;
         Context : Context_Type :=
            (Container => Container'Unrestricted_Access);
      begin
         Array_Sorting.In_Place_Merge_Sort (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Container'Last),
            Context'Address,
            LT => LT'Access,
            Swap => Swap'Access);
      end;
   else
      declare
         type Context_Type is limited record
            Container : not null access Array_Type;
            Offset : Long_Long_Integer;
         end record;
         pragma Suppress_Initialization (Context_Type);
         function LT (Left, Right : Integer; Params : System.Address)
            return Boolean;
         function LT (Left, Right : Integer; Params : System.Address)
            return Boolean
         is
            Context : Context_Type;
            for Context'Address use Params;
            Left_Index : constant Index_Type :=
               Index_Type'Val (Long_Long_Integer (Left) + Context.Offset);
            Right_Index : constant Index_Type :=
               Index_Type'Val (Long_Long_Integer (Right) + Context.Offset);
         begin
            return Context.Container (Left_Index) <
               Context.Container (Right_Index);
         end LT;
         procedure Swap (Left, Right : Integer; Params : System.Address);
         procedure Swap (Left, Right : Integer; Params : System.Address) is
            Context : Context_Type;
            for Context'Address use Params;
            Left_Index : constant Index_Type :=
               Index_Type'Val (Long_Long_Integer (Left) + Context.Offset);
            Right_Index : constant Index_Type :=
               Index_Type'Val (Long_Long_Integer (Right) + Context.Offset);
            Temp : constant Element_Type := Context.Container (Left_Index);
         begin
            Context.Container (Left_Index) := Context.Container (Right_Index);
            Context.Container (Right_Index) := Temp;
         end Swap;
         Offset : constant Long_Long_Integer :=
            Index_Type'Pos (Container'First);
         Context : Context_Type := (Container'Unrestricted_Access, Offset);
      begin
         Array_Sorting.In_Place_Merge_Sort (
            0,
            Integer (Index_Type'Pos (Container'Last) - Offset),
            Context'Address,
            LT => LT'Access,
            Swap => Swap'Access);
      end;
   end if;
end Ada.Containers.Generic_Array_Sort;
