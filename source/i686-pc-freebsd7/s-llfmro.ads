pragma License (Unrestricted);
--  implementation package
function System.Long_Long_Float_Machine_Rounding (X : Long_Long_Float)
   return Long_Long_Float;
pragma Import (Intrinsic, System.Long_Long_Float_Machine_Rounding,
   "__builtin_roundl");
--  FreeBSD does not have nearbyintl,
--  It should be replaced by inline-assember ?
pragma Pure (System.Long_Long_Float_Machine_Rounding);
