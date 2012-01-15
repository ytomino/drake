with Ada.Float.Elementary_Functions;
package body Ada.Numerics.Generic_Complex_Types.Inside is
   pragma Suppress (All_Checks);

   function Arctan is new Float.Elementary_Functions.Arctan (Real'Base);
   subtype Float is Standard.Float; -- hiding "Float" package

   --  implementation

   function Argument (X : Complex) return Real'Base is
   begin
      --  FreeBSD does not have carg
      return Arctan (X.Im, X.Re);
   end Argument;

   function Modulus (X : Complex) return Real'Base is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function cabsf (A1 : Complex) return Float;
            pragma Import (Intrinsic, cabsf, "__builtin_cabsf");
         begin
            return Real'Base (cabsf (X));
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function cabs (A1 : Complex) return Long_Float;
            pragma Import (Intrinsic, cabs, "__builtin_cabs");
         begin
            return Real'Base (cabs (X));
         end;
      else
         --  FreeBSD does not have cabsl
         declare
            function sqrt (A1 : Long_Float) return Long_Float;
            pragma Import (Intrinsic, sqrt, "__builtin_sqrt");
         begin
            return Real'Base (sqrt (Long_Float (X.Re * X.Re + X.Im * X.Im)));
         end;
      end if;
   end Modulus;

end Ada.Numerics.Generic_Complex_Types.Inside;
