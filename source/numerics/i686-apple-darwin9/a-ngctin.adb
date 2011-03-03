package body Ada.Numerics.Generic_Complex_Types.Inside is

   function Argument (X : Complex) return Real'Base is
      function cargf (A1 : Complex) return Float;
      pragma Import (Intrinsic, cargf, "__builtin_cargf");
      function carg (A1 : Complex) return Long_Float;
      pragma Import (Intrinsic, carg, "__builtin_carg");
      function cargl (A1 : Complex) return Long_Long_Float;
      pragma Import (Intrinsic, cargl, "__builtin_cargl");
   begin
      if Real'Digits <= Float'Digits then
         return Real'Base (cargf (X));
      elsif Real'Digits <= Long_Float'Digits then
         return Real'Base (carg (X));
      else
         return Real'Base (cargl (X));
      end if;
   end Argument;

end Ada.Numerics.Generic_Complex_Types.Inside;
