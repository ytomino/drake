pragma License (Unrestricted);
--  implementation unit
generic
   type Float_Type is digits <>;
function Ada.Numerics.Generic_Elementary_Cos (X : Float_Type'Base)
   return Float_Type'Base;
pragma Pure (Generic_Elementary_Cos);
pragma Inline (Generic_Elementary_Cos);
