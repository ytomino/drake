pragma License (Unrestricted);
--  implementation unit
function System.Long_Long_Float_Machine_Rounding (X : Long_Long_Float)
   return Long_Long_Float;
pragma Import (Intrinsic, System.Long_Long_Float_Machine_Rounding,
   "__builtin_nearbyintl");
pragma Pure (System.Long_Long_Float_Machine_Rounding);
