function System.Long_Long_Float_Machine_Rounding (X : Long_Long_Float)
   return Long_Long_Float
is
   function nearbyintl (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, nearbyintl, "__builtin_nearbyintl");
begin
   return nearbyintl (X);
end System.Long_Long_Float_Machine_Rounding;
