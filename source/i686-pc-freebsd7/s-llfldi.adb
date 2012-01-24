procedure System.Long_Long_Float_Divide (
   Left, Right : Long_Long_Float;
   Quotient, Remainder : out Long_Long_Float)
is
   pragma Suppress (All_Checks);
   function truncl (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, truncl, "__builtin_truncl");
begin
   Quotient := truncl (Left / Right);
   Remainder := Left - Quotient * Right;
end System.Long_Long_Float_Divide;
