pragma License (Unrestricted);
--  Ada 2012
generic
   type Index_Type is (<>);
   with function Before (Left, Right : Index_Type) return Boolean;
   with procedure Swap (Left, Right : Index_Type);
procedure Ada.Containers.Generic_Sort (First, Last : Index_Type'Base);
pragma Pure (Ada.Containers.Generic_Sort);
