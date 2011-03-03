package body Ada.Numerics.Generic_Complex_Elementary_Functions is
   pragma Suppress (All_Checks);

   function Arccos (X : Complex) return Complex is
      function cacosf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, cacosf, "__builtin_cacosf");
      function cacos (A1 : Complex) return Complex;
      pragma Import (Intrinsic, cacos, "__builtin_cacos");
      function cacosl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, cacosl, "__builtin_cacosl");
   begin
      if Real'Digits <= Float'Digits then
         return cacosf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return cacos (X);
      else
         return cacosl (X);
      end if;
   end Arccos;

   function Arccosh (X : Complex) return Complex is
      function cacoshf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, cacoshf, "__builtin_cacoshf");
      function cacosh (A1 : Complex) return Complex;
      pragma Import (Intrinsic, cacosh, "__builtin_cacosh");
      function cacoshl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, cacoshl, "__builtin_cacoshl");
   begin
      if Real'Digits <= Float'Digits then
         return cacoshf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return cacosh (X);
      else
         return cacoshl (X);
      end if;
   end Arccosh;

   function Arccot (X : Complex) return Complex is
   begin
      return Pi / 2.0 - Arctan (X);
   end Arccot;

   function Arccoth (X : Complex) return Complex is
   begin
      if X.Re = 0.0 and then X.Im = 0.0 then
         return (0.0, Pi / 2.0);
      else
         return Arctanh (1.0 / X);
      end if;
   end Arccoth;

   function Arcsin (X : Complex) return Complex is
      function casinf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, casinf, "__builtin_casinf");
      function casin (A1 : Complex) return Complex;
      pragma Import (Intrinsic, casin, "__builtin_casin");
      function casinl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, casinl, "__builtin_casinl");
   begin
      if Real'Digits <= Float'Digits then
         return casinf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return casin (X);
      else
         return casinl (X);
      end if;
   end Arcsin;

   function Arcsinh (X : Complex) return Complex is
      function casinhf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, casinhf, "__builtin_casinhf");
      function casinh (A1 : Complex) return Complex;
      pragma Import (Intrinsic, casinh, "__builtin_casinh");
      function casinhl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, casinhl, "__builtin_casinhl");
   begin
      if Real'Digits <= Float'Digits then
         return casinhf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return casinh (X);
      else
         return casinhl (X);
      end if;
   end Arcsinh;

   function Arctan (X : Complex) return Complex is
      function catanf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, catanf, "__builtin_catanf");
      function catan (A1 : Complex) return Complex;
      pragma Import (Intrinsic, catan, "__builtin_catan");
      function catanl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, catanl, "__builtin_catanl");
   begin
      if Real'Digits <= Float'Digits then
         return catanf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return catan (X);
      else
         return catanl (X);
      end if;
   end Arctan;

   function Arctanh (X : Complex) return Complex is
      function catanhf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, catanhf, "__builtin_catanhf");
      function catanh (A1 : Complex) return Complex;
      pragma Import (Intrinsic, catanh, "__builtin_catanh");
      function catanhl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, catanhl, "__builtin_catanhl");
   begin
      if Real'Digits <= Float'Digits then
         return catanhf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return catanh (X);
      else
         return catanhl (X);
      end if;
   end Arctanh;

   function Cos (X : Complex) return Complex is
      function ccosf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ccosf, "__builtin_ccosf");
      function ccos (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ccos, "__builtin_ccos");
      function ccosl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ccosl, "__builtin_ccosl");
   begin
      if Real'Digits <= Float'Digits then
         return ccosf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return ccos (X);
      else
         return ccosl (X);
      end if;
   end Cos;

   function Cosh (X : Complex) return Complex is
      function ccoshf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ccoshf, "__builtin_ccoshf");
      function ccosh (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ccosh, "__builtin_ccosh");
      function ccoshl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ccoshl, "__builtin_ccoshl");
   begin
      if Real'Digits <= Float'Digits then
         return ccoshf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return ccosh (X);
      else
         return ccoshl (X);
      end if;
   end Cosh;

   function Cot (X : Complex) return Complex is
   begin
      return Cos (X) / Sin (X);
   end Cot;

   function Coth (X : Complex) return Complex is
   begin
      return Cosh (X) / Sinh (X);
   end Coth;

   function Exp (X : Complex) return Complex is
      function cexpf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, cexpf, "__builtin_cexpf");
      function cexp (A1 : Complex) return Complex;
      pragma Import (Intrinsic, cexp, "__builtin_cexp");
      function cexpl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, cexpl, "__builtin_cexpl");
   begin
      if Real'Digits <= Float'Digits then
         return cexpf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return cexp (X);
      else
         return cexpl (X);
      end if;
   end Exp;

   function Exp (X : Imaginary) return Complex is
      function cexpif (A1 : Float) return Complex;
      pragma Import (Intrinsic, cexpif, "__builtin_cexpif");
      function cexpi (A1 : Long_Float) return Complex;
      pragma Import (Intrinsic, cexpi, "__builtin_cexpi");
      function cexpil (A1 : Long_Long_Float) return Complex;
      pragma Import (Intrinsic, cexpil, "__builtin_cexpil");
   begin
      if Real'Digits <= Float'Digits then
         return cexpif (Float (Im (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return cexpi (Long_Float (Im (X)));
      else
         return cexpil (Long_Long_Float (Im (X)));
      end if;
   end Exp;

   function Log (X : Complex) return Complex is
      function clogf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, clogf, "__builtin_clogf");
      function clog (A1 : Complex) return Complex;
      pragma Import (Intrinsic, clog, "__builtin_clog");
      function clogl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, clogl, "__builtin_clogl");
   begin
      if Real'Digits <= Float'Digits then
         return clogf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return clog (X);
      else
         return clogl (X);
      end if;
   end Log;

   function Sin (X : Complex) return Complex is
      function csinf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, csinf, "__builtin_csinf");
      function csin (A1 : Complex) return Complex;
      pragma Import (Intrinsic, csin, "__builtin_csin");
      function csinl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, csinl, "__builtin_csinl");
   begin
      if Real'Digits <= Float'Digits then
         return csinf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return csin (X);
      else
         return csinl (X);
      end if;
   end Sin;

   function Sinh (X : Complex) return Complex is
      function csinhf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, csinhf, "__builtin_csinhf");
      function csinh (A1 : Complex) return Complex;
      pragma Import (Intrinsic, csinh, "__builtin_csinh");
      function csinhl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, csinhl, "__builtin_csinhl");
   begin
      if Real'Digits <= Float'Digits then
         return csinhf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return csinh (X);
      else
         return csinhl (X);
      end if;
   end Sinh;

   function Sqrt (X : Complex) return Complex is
      function csqrtf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, csqrtf, "__builtin_csqrtf");
      function csqrt (A1 : Complex) return Complex;
      pragma Import (Intrinsic, csqrt, "__builtin_csqrt");
      function csqrtl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, csqrtl, "__builtin_csqrtl");
   begin
      if Real'Digits <= Float'Digits then
         return csqrtf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return csqrt (X);
      else
         return csqrtl (X);
      end if;
   end Sqrt;

   function Tan (X : Complex) return Complex is
      function ctanf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ctanf, "__builtin_ctanf");
      function ctan (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ctan, "__builtin_ctan");
      function ctanl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ctanl, "__builtin_ctanl");
   begin
      if Real'Digits <= Float'Digits then
         return ctanf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return ctan (X);
      else
         return ctanl (X);
      end if;
   end Tan;

   function Tanh (X : Complex) return Complex is
      function ctanhf (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ctanhf, "__builtin_ctanhf");
      function ctanh (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ctanh, "__builtin_ctanh");
      function ctanhl (A1 : Complex) return Complex;
      pragma Import (Intrinsic, ctanhl, "__builtin_ctanhl");
   begin
      if Real'Digits <= Float'Digits then
         return ctanhf (X);
      elsif Real'Digits <= Long_Float'Digits then
         return ctanh (X);
      else
         return ctanhl (X);
      end if;
   end Tanh;

   function "**" (Left : Complex; Right : Complex) return Complex is
      function cpowf (A1, A2 : Complex) return Complex;
      pragma Import (Intrinsic, cpowf, "__builtin_cpowf");
      function cpow (A1, A2 : Complex) return Complex;
      pragma Import (Intrinsic, cpow, "__builtin_cpow");
      function cpowl (A1, A2 : Complex) return Complex;
      pragma Import (Intrinsic, cpowl, "__builtin_cpowl");
   begin
      if not Standard'Fast_Math
         and then Left.Re = 0.0 and then Left.Im = 0.0
         and then Right.Re = 0.0 and then Right.Im = 0.0
      then
         raise Argument_Error; -- CXG1004
      elsif Right.Re = 1.0 and Right.Im = 0.0 then
         return Left; -- CXG1005
      elsif Real'Digits <= Float'Digits then
         return cpowf (Left, Right);
      elsif Real'Digits <= Long_Float'Digits then
         return cpow (Left, Right);
      else
         return cpowl (Left, Right);
      end if;
   end "**";

   function "**" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return Left ** (Re => Right, Im => 0.0);
   end "**";

   function "**" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left, Im => 0.0) ** Right;
   end "**";

end Ada.Numerics.Generic_Complex_Elementary_Functions;
