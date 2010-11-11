pragma License (Unrestricted);
generic
   type Index_Type is (<>);
   type Element_Type is private;
   type Array_Type is array (Index_Type) of Element_Type;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
procedure Ada.Containers.Generic_Constrained_Array_Sort (
   Container : in out Array_Type);
pragma Pure (Ada.Containers.Generic_Constrained_Array_Sort);
