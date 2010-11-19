package body Ada.Numerics.Generic_Elementary_Functions is
   pragma Suppress (All_Checks);

   function Arccos (X : Float_Type'Base) return Float_Type'Base is
      function acosf (A1 : Float) return Float;
      pragma Import (Intrinsic, acosf, "__builtin_acosf");
      function acos (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, acos, "__builtin_acos");
      function acosl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, acosl, "__builtin_acosl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (acosf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (acos (Long_Float (X)));
      else
         return Float_Type (acosl (Long_Long_Float (X)));
      end if;
   end Arccos;

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      return Arccos (X) * Cycle / (2.0 * Pi);
   end Arccos;

   function Arccosh (X : Float_Type'Base) return Float_Type'Base is
      function acoshf (A1 : Float) return Float;
      pragma Import (Intrinsic, acoshf, "__builtin_acoshf");
      function acosh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, acosh, "__builtin_acosh");
      function acoshl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, acoshl, "__builtin_acoshl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (acoshf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (acosh (Long_Float (X)));
      else
         return Float_Type (acoshl (Long_Long_Float (X)));
      end if;
   end Arccosh;

   function Arccot (X : Float_Type'Base;
                    Y : Float_Type'Base := 1.0) return Float_Type'Base is
   begin
      return Arctan (Y, X);
   end Arccot;

   function Arccot (
      X : Float_Type'Base;
      Y : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return Float_Type'Base is
   begin
      return Arccot (X, Y) * Cycle / (2.0 * Pi);
   end Arccot;

   function Arccoth (X : Float_Type'Base) return Float_Type'Base is
   begin
      return Log ((X + 1.0) / (X - 1.0)) * 0.5;
   end Arccoth;

   function Arcsin (X : Float_Type'Base) return Float_Type'Base is
      function asinf (A1 : Float) return Float;
      pragma Import (Intrinsic, asinf, "__builtin_asinf");
      function asin (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, asin, "__builtin_asin");
      function asinl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, asinl, "__builtin_asinl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (asinf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (asin (Long_Float (X)));
      else
         return Float_Type (asinl (Long_Long_Float (X)));
      end if;
   end Arcsin;

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      return Arcsin (X) * Cycle / (2.0 * Pi);
   end Arcsin;

   function Arcsinh (X : Float_Type'Base) return Float_Type'Base is
      function asinhf (A1 : Float) return Float;
      pragma Import (Intrinsic, asinhf, "__builtin_asinhf");
      function asinh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, asinh, "__builtin_asinh");
      function asinhl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, asinhl, "__builtin_asinhl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (asinhf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (asinh (Long_Float (X)));
      else
         return Float_Type (asinhl (Long_Long_Float (X)));
      end if;
   end Arcsinh;

   function Arctan (Y : Float_Type'Base;
                    X : Float_Type'Base := 1.0) return Float_Type'Base
   is
      function atan2f (A1, A2 : Float) return Float;
      pragma Import (Intrinsic, atan2f, "__builtin_atan2f");
      function atan2 (A1, A2 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, atan2, "__builtin_atan2");
      function atan2l (A1, A2 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, atan2l, "__builtin_atan2l");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (atan2f (Float (Y), Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (atan2 (Long_Float (Y), Long_Float (X)));
      else
         return Float_Type (atan2l (Long_Long_Float (Y), Long_Long_Float (X)));
      end if;
   end Arctan;

   function Arctan (
      Y : Float_Type'Base;
      X : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return Float_Type'Base is
   begin
      return Arctan (Y, X) * Cycle / (2.0 * Pi);
   end Arctan;

   function Arctanh (X : Float_Type'Base) return Float_Type'Base is
      function atanhf (A1 : Float) return Float;
      pragma Import (Intrinsic, atanhf, "__builtin_atanhf");
      function atanh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, atanh, "__builtin_atanh");
      function atanhl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, atanhl, "__builtin_atanhl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (atanhf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (atanh (Long_Float (X)));
      else
         return Float_Type (atanhl (Long_Long_Float (X)));
      end if;
   end Arctanh;

   function Cos (X : Float_Type'Base) return Float_Type'Base is
      function cosf (A1 : Float) return Float;
      pragma Import (Intrinsic, cosf, "__builtin_cosf");
      function cos (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, cos, "__builtin_cos");
      function cosl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, cosl, "__builtin_cosl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (cosf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (cos (Long_Float (X)));
      else
         return Float_Type (cosl (Long_Long_Float (X)));
      end if;
   end Cos;

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      return Cos (X * (2.0 * Pi) / Cycle);
   end Cos;

   function Cosh (X : Float_Type'Base) return Float_Type'Base is
      function coshf (A1 : Float) return Float;
      pragma Import (Intrinsic, coshf, "__builtin_coshf");
      function cosh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, cosh, "__builtin_cosh");
      function coshl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, coshl, "__builtin_coshl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (coshf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (cosh (Long_Float (X)));
      else
         return Float_Type (coshl (Long_Long_Float (X)));
      end if;
   end Cosh;

   function Cot (X : Float_Type'Base) return Float_Type'Base is
   begin
      return 1.0 / Tan (X);
   end Cot;

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      return Cot (X * (2.0 * Pi) / Cycle);
   end Cot;

   function Coth (X : Float_Type'Base) return Float_Type'Base is
   begin
      return 1.0 / Tanh (X);
   end Coth;

   function Exp (X : Float_Type'Base) return Float_Type'Base is
      function expf (A1 : Float) return Float;
      pragma Import (Intrinsic, expf, "__builtin_expf");
      function exp (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, exp, "__builtin_exp");
      function expl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, expl, "__builtin_expl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (expf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (exp (Long_Float (X)));
      else
         return Float_Type (expl (Long_Long_Float (X)));
      end if;
   end Exp;

   function Log (X : Float_Type'Base) return Float_Type'Base is
      function logf (A1 : Float) return Float;
      pragma Import (Intrinsic, logf, "__builtin_logf");
      function log (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, log, "__builtin_log");
      function logl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, logl, "__builtin_logl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (logf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (log (Long_Float (X)));
      else
         return Float_Type (logl (Long_Long_Float (X)));
      end if;
   end Log;

   function Log (X, Base : Float_Type'Base) return Float_Type'Base is
   begin
      return Log (X) / Log (Base);
   end Log;

   function Sin (X : Float_Type'Base) return Float_Type'Base is
      function sinf (A1 : Float) return Float;
      pragma Import (Intrinsic, sinf, "__builtin_sinf");
      function sin (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, sin, "__builtin_sin");
      function sinl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, sinl, "__builtin_sinl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (sinf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (sin (Long_Float (X)));
      else
         return Float_Type (sinl (Long_Long_Float (X)));
      end if;
   end Sin;

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      return Sin (X * (2.0 * Pi) / Cycle);
   end Sin;

   function Sinh (X : Float_Type'Base) return Float_Type'Base is
      function sinhf (A1 : Float) return Float;
      pragma Import (Intrinsic, sinhf, "__builtin_sinhf");
      function sinh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, sinh, "__builtin_sinh");
      function sinhl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, sinhl, "__builtin_sinhl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (sinhf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (sinh (Long_Float (X)));
      else
         return Float_Type (sinhl (Long_Long_Float (X)));
      end if;
   end Sinh;

   function Sqrt (X : Float_Type'Base) return Float_Type'Base is
      function sqrtf (A1 : Float) return Float;
      pragma Import (Intrinsic, sqrtf, "__builtin_sqrtf");
      function sqrt (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, sqrt, "__builtin_sqrt");
      function sqrtl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, sqrtl, "__builtin_sqrtl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (sqrtf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (sqrt (Long_Float (X)));
      else
         return Float_Type (sqrtl (Long_Long_Float (X)));
      end if;
   end Sqrt;

   function Tan (X : Float_Type'Base) return Float_Type'Base is
      function tanf (A1 : Float) return Float;
      pragma Import (Intrinsic, tanf, "__builtin_tanf");
      function tan (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, tan, "__builtin_tan");
      function tanl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, tanl, "__builtin_tanl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (tanf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (tan (Long_Float (X)));
      else
         return Float_Type (tanl (Long_Long_Float (X)));
      end if;
   end Tan;

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      return Tan (X * (2.0 * Pi) / Cycle);
   end Tan;

   function Tanh (X : Float_Type'Base) return Float_Type'Base is
      function tanhf (A1 : Float) return Float;
      pragma Import (Intrinsic, tanhf, "__builtin_tanhf");
      function tanh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, tanh, "__builtin_tanh");
      function tanhl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, tanhl, "__builtin_tanhl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (tanhf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (tanh (Long_Float (X)));
      else
         return Float_Type (tanhl (Long_Long_Float (X)));
      end if;
   end Tanh;

   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base is
      function powf (A1, A2 : Float) return Float;
      pragma Import (Intrinsic, powf, "__builtin_powf");
      function pow (A1, A2 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, pow, "__builtin_pow");
      function powl (A1, A2 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, powl, "__builtin_powl");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (powf (Float (Left), Float (Right)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (pow (Long_Float (Left), Long_Float (Right)));
      else
         return Float_Type (powl (Long_Long_Float (Left),
                                  Long_Long_Float (Right)));
      end if;
   end "**";

end Ada.Numerics.Generic_Elementary_Functions;
