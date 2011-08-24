function System.Long_Long_Float_Machine_Rounding (X : Long_Long_Float)
   return Long_Long_Float
is
   pragma Suppress (All_Checks);
   --  FreeBSD does not have nearbyintl,
   --  It should be replaced by inline-assember ?
   function roundl (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, roundl, "__builtin_roundl");
begin
   return roundl (X);
end System.Long_Long_Float_Machine_Rounding;
