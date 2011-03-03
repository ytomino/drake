function Ada.Numerics.Generic_Elementary_Arctan (
   Y : Float_Type'Base;
   X : Float_Type'Base := 1.0)
   return Float_Type'Base
is
   function atan2f (A1, A2 : Float) return Float;
   pragma Import (Intrinsic, atan2f, "__builtin_atan2f");
   function atan2 (A1, A2 : Long_Float) return Long_Float;
   pragma Import (Intrinsic, atan2, "__builtin_atan2");
   function atan2l (A1, A2 : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, atan2l, "__builtin_atan2l");
begin
   if not Standard'Fast_Math and then X = 0.0 and then Y = 0.0 then
      raise Argument_Error; -- CXA5A07
   elsif Float_Type'Digits <= Float'Digits then
      return Float_Type (atan2f (Float (Y), Float (X)));
   elsif Float_Type'Digits <= Long_Float'Digits then
      return Float_Type (atan2 (Long_Float (Y), Long_Float (X)));
   else
      return Float_Type (atan2l (Long_Long_Float (Y), Long_Long_Float (X)));
   end if;
end Ada.Numerics.Generic_Elementary_Arctan;
