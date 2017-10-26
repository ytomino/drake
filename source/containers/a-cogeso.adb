with Ada.Containers.Array_Sorting;
with System.Long_Long_Integer_Types;
procedure Ada.Containers.Generic_Sort (First, Last : Index_Type'Base) is
   subtype Word_Integer is System.Long_Long_Integer_Types.Word_Integer;
begin
   if Index_Type'Pos (Index_Type'First) in
         Long_Long_Integer (Word_Integer'First) ..
         Long_Long_Integer (Word_Integer'Last)
      and then Index_Type'Pos (Index_Type'Last) in
         Long_Long_Integer (Word_Integer'First) ..
         Long_Long_Integer (Word_Integer'Last)
   then
      declare
         function LT (Left, Right : Word_Integer; Params : System.Address)
            return Boolean;
         function LT (Left, Right : Word_Integer; Params : System.Address)
            return Boolean
         is
            pragma Unreferenced (Params);
            Left_Index : constant Index_Type := Index_Type'Val (Left);
            Right_Index : constant Index_Type := Index_Type'Val (Right);
         begin
            return Before (Left_Index, Right_Index);
         end LT;
         procedure Swap (
            Left, Right : Word_Integer;
            Params : System.Address);
         procedure Swap (
            Left, Right : Word_Integer;
            Params : System.Address)
         is
            pragma Unreferenced (Params);
            Left_Index : constant Index_Type := Index_Type'Val (Left);
            Right_Index : constant Index_Type := Index_Type'Val (Right);
         begin
            Swap (Left_Index, Right_Index);
         end Swap;
      begin
         Array_Sorting.In_Place_Merge_Sort (
            Index_Type'Pos (First),
            Index_Type'Pos (Last),
            System.Null_Address,
            LT => LT'Access,
            Swap => Swap'Access);
      end;
   else
      declare
         type Context_Type is limited record
            Offset : Long_Long_Integer;
         end record;
         pragma Suppress_Initialization (Context_Type);
         function LT (Left, Right : Word_Integer; Params : System.Address)
            return Boolean;
         function LT (Left, Right : Word_Integer; Params : System.Address)
            return Boolean
         is
            Context : Context_Type;
            for Context'Address use Params;
            Left_Index : constant Index_Type :=
               Index_Type'Val (Long_Long_Integer (Left) + Context.Offset);
            Right_Index : constant Index_Type :=
               Index_Type'Val (Long_Long_Integer (Right) + Context.Offset);
         begin
            return Before (Left_Index, Right_Index);
         end LT;
         procedure Swap (
            Left, Right : Word_Integer;
            Params : System.Address);
         procedure Swap (
            Left, Right : Word_Integer;
            Params : System.Address)
         is
            Context : Context_Type;
            for Context'Address use Params;
            Left_Index : constant Index_Type :=
               Index_Type'Val (Long_Long_Integer (Left) + Context.Offset);
            Right_Index : constant Index_Type :=
               Index_Type'Val (Long_Long_Integer (Right) + Context.Offset);
         begin
            Swap (Left_Index, Right_Index);
         end Swap;
         Offset : constant Long_Long_Integer := Index_Type'Pos (First);
         Context : Context_Type := (Offset => Offset);
      begin
         Array_Sorting.In_Place_Merge_Sort (
            0,
            Word_Integer (Long_Long_Integer'(Index_Type'Pos (Last) - Offset)),
            Context'Address,
            LT => LT'Access,
            Swap => Swap'Access);
      end;
   end if;
end Ada.Containers.Generic_Sort;
