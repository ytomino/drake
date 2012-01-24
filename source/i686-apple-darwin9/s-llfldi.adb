procedure System.Long_Long_Float_Divide (
   Left, Right : Long_Long_Float;
   Quotient, Remainder : out Long_Long_Float)
is
   pragma Suppress (All_Checks);
   function truncl (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, truncl, "__builtin_truncl");
   function fmodl (x, y : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, fmodl, "__builtin_fmodl");
begin
   Quotient := truncl (Left / Right);
   Remainder := fmodl (Left, Right);
end System.Long_Long_Float_Divide;
