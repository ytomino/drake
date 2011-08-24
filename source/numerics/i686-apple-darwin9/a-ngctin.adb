package body Ada.Numerics.Generic_Complex_Types.Inside is
   pragma Suppress (All_Checks);

   function Argument (X : Complex) return Real'Base is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function cargf (A1 : Complex) return Float;
            pragma Import (Intrinsic, cargf, "__builtin_cargf");
         begin
            return Real'Base (cargf (X));
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function carg (A1 : Complex) return Long_Float;
            pragma Import (Intrinsic, carg, "__builtin_carg");
         begin
            return Real'Base (carg (X));
         end;
      else
         declare
            function cargl (A1 : Complex) return Long_Long_Float;
            pragma Import (Intrinsic, cargl, "__builtin_cargl");
         begin
            return Real'Base (cargl (X));
         end;
      end if;
   end Argument;

end Ada.Numerics.Generic_Complex_Types.Inside;
