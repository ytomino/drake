procedure System.Long_Long_Float_Divide (
   Left, Right : Long_Long_Float;
   Quotient, Remainder : out Long_Long_Float) is
begin
   Quotient := Long_Long_Float'Truncation (Left / Right);
   Remainder := Left - Quotient * Right;
end System.Long_Long_Float_Divide;
