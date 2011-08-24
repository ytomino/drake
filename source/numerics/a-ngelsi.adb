function Ada.Numerics.Generic_Elementary_Sin (X : Float_Type'Base)
   return Float_Type'Base
is
   pragma Suppress (All_Checks);
begin
   if Float_Type'Digits <= Float'Digits then
      declare
         function sinf (A1 : Float) return Float;
         pragma Import (Intrinsic, sinf, "__builtin_sinf");
      begin
         return Float_Type (sinf (Float (X)));
      end;
   elsif Float_Type'Digits <= Long_Float'Digits then
      declare
         function sin (A1 : Long_Float) return Long_Float;
         pragma Import (Intrinsic, sin, "__builtin_sin");
      begin
         return Float_Type (sin (Long_Float (X)));
      end;
   else
      declare
         function sinl (A1 : Long_Long_Float) return Long_Long_Float;
         pragma Import (Intrinsic, sinl, "__builtin_sinl");
      begin
         return Float_Type (sinl (Long_Long_Float (X)));
      end;
   end if;
end Ada.Numerics.Generic_Elementary_Sin;
