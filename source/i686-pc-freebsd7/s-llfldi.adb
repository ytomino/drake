procedure System.Long_Long_Float_Divide (
   Left, Right : Long_Long_Float;
   Quotient, Remainder : out Long_Long_Float)
is
   pragma Suppress (All_Checks);
begin
   Quotient := Long_Long_Float'Truncation (Left / Right);
   Remainder := Left - Quotient * Right;
end System.Long_Long_Float_Divide;
