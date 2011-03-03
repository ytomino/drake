pragma License (Unrestricted);
--  implementation unit
generic
   type Float_Type is digits <>;
function Ada.Numerics.Generic_Elementary_Arctan (
   Y : Float_Type'Base;
   X : Float_Type'Base := 1.0)
   return Float_Type'Base;
pragma Pure (Generic_Elementary_Arctan);
pragma Inline (Generic_Elementary_Arctan);
