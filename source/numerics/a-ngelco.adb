function Ada.Numerics.Generic_Elementary_Cos (X : Float_Type'Base)
   return Float_Type'Base is
begin
   if Float_Type'Digits <= Float'Digits then
      declare
         function cosf (A1 : Float) return Float;
         pragma Import (Intrinsic, cosf, "__builtin_cosf");
      begin
         return Float_Type (cosf (Float (X)));
      end;
   elsif Float_Type'Digits <= Long_Float'Digits then
      declare
         function cos (A1 : Long_Float) return Long_Float;
         pragma Import (Intrinsic, cos, "__builtin_cos");
      begin
         return Float_Type (cos (Long_Float (X)));
      end;
   else
      declare
         function cosl (A1 : Long_Long_Float) return Long_Long_Float;
         pragma Import (Intrinsic, cosl, "__builtin_cosl");
      begin
         return Float_Type (cosl (Long_Long_Float (X)));
      end;
   end if;
end Ada.Numerics.Generic_Elementary_Cos;
