procedure System.Long_Long_Float_Divide (
   Left, Right : Long_Long_Float;
   Quotient, Remainder : out Long_Long_Float)
is
   pragma Suppress (All_Checks);
   function remainderl (x, y : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, remainderl, "__builtin_remainderl");
begin
   Quotient := Long_Long_Float'Truncation (Left / Right);
   Remainder := remainderl (Left, Right);
   if Remainder < 0.0 and then Left > 0.0 then
      Remainder := Remainder + abs Right;
   elsif Remainder > 0.0 and then Left < 0.0 then
      Remainder := Remainder - abs Right;
   end if;
end System.Long_Long_Float_Divide;
