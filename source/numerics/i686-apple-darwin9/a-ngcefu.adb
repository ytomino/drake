package body Ada.Numerics.Generic_Complex_Elementary_Functions is
   pragma Suppress (All_Checks);

   function Arccos (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function cacosf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, cacosf, "__builtin_cacosf");
         begin
            return cacosf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function cacos (A1 : Complex) return Complex;
            pragma Import (Intrinsic, cacos, "__builtin_cacos");
         begin
            return cacos (X);
         end;
      else
         declare
            function cacosl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, cacosl, "__builtin_cacosl");
         begin
            return cacosl (X);
         end;
      end if;
   end Arccos;

   function Arccosh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function cacoshf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, cacoshf, "__builtin_cacoshf");
         begin
            return cacoshf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function cacosh (A1 : Complex) return Complex;
            pragma Import (Intrinsic, cacosh, "__builtin_cacosh");
         begin
            return cacosh (X);
         end;
      else
         declare
            function cacoshl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, cacoshl, "__builtin_cacoshl");
         begin
            return cacoshl (X);
         end;
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
   begin
      if Real'Digits <= Float'Digits then
         declare
            function casinf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, casinf, "__builtin_casinf");
         begin
            return casinf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function casin (A1 : Complex) return Complex;
            pragma Import (Intrinsic, casin, "__builtin_casin");
         begin
            return casin (X);
         end;
      else
         declare
            function casinl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, casinl, "__builtin_casinl");
         begin
            return casinl (X);
         end;
      end if;
   end Arcsin;

   function Arcsinh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function casinhf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, casinhf, "__builtin_casinhf");
         begin
            return casinhf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function casinh (A1 : Complex) return Complex;
            pragma Import (Intrinsic, casinh, "__builtin_casinh");
         begin
            return casinh (X);
         end;
      else
         declare
            function casinhl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, casinhl, "__builtin_casinhl");
         begin
            return casinhl (X);
         end;
      end if;
   end Arcsinh;

   function Arctan (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function catanf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, catanf, "__builtin_catanf");
         begin
            return catanf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function catan (A1 : Complex) return Complex;
            pragma Import (Intrinsic, catan, "__builtin_catan");
         begin
            return catan (X);
         end;
      else
         declare
            function catanl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, catanl, "__builtin_catanl");
         begin
            return catanl (X);
         end;
      end if;
   end Arctan;

   function Arctanh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function catanhf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, catanhf, "__builtin_catanhf");
         begin
            return catanhf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function catanh (A1 : Complex) return Complex;
            pragma Import (Intrinsic, catanh, "__builtin_catanh");
         begin
            return catanh (X);
         end;
      else
         declare
            function catanhl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, catanhl, "__builtin_catanhl");
         begin
            return catanhl (X);
         end;
      end if;
   end Arctanh;

   function Cos (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function ccosf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ccosf, "__builtin_ccosf");
         begin
            return ccosf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function ccos (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ccos, "__builtin_ccos");
         begin
            return ccos (X);
         end;
      else
         declare
            function ccosl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ccosl, "__builtin_ccosl");
         begin
            return ccosl (X);
         end;
      end if;
   end Cos;

   function Cosh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function ccoshf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ccoshf, "__builtin_ccoshf");
         begin
            return ccoshf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function ccosh (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ccosh, "__builtin_ccosh");
         begin
            return ccosh (X);
         end;
      else
         declare
            function ccoshl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ccoshl, "__builtin_ccoshl");
         begin
            return ccoshl (X);
         end;
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
   begin
      if Real'Digits <= Float'Digits then
         declare
            function cexpf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, cexpf, "__builtin_cexpf");
         begin
            return cexpf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function cexp (A1 : Complex) return Complex;
            pragma Import (Intrinsic, cexp, "__builtin_cexp");
         begin
            return cexp (X);
         end;
      else
         declare
            function cexpl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, cexpl, "__builtin_cexpl");
         begin
            return cexpl (X);
         end;
      end if;
   end Exp;

   function Exp (X : Imaginary) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function cexpif (A1 : Float) return Complex;
            pragma Import (Intrinsic, cexpif, "__builtin_cexpif");
         begin
            return cexpif (Float (Im (X)));
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function cexpi (A1 : Long_Float) return Complex;
            pragma Import (Intrinsic, cexpi, "__builtin_cexpi");
         begin
            return cexpi (Long_Float (Im (X)));
         end;
      else
         declare
            function cexpil (A1 : Long_Long_Float) return Complex;
            pragma Import (Intrinsic, cexpil, "__builtin_cexpil");
         begin
            return cexpil (Long_Long_Float (Im (X)));
         end;
      end if;
   end Exp;

   function Log (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function clogf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, clogf, "__builtin_clogf");
         begin
            return clogf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function clog (A1 : Complex) return Complex;
            pragma Import (Intrinsic, clog, "__builtin_clog");
         begin
            return clog (X);
         end;
      else
         declare
            function clogl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, clogl, "__builtin_clogl");
         begin
            return clogl (X);
         end;
      end if;
   end Log;

   function Sin (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function csinf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, csinf, "__builtin_csinf");
         begin
            return csinf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function csin (A1 : Complex) return Complex;
            pragma Import (Intrinsic, csin, "__builtin_csin");
         begin
            return csin (X);
         end;
      else
         declare
            function csinl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, csinl, "__builtin_csinl");
         begin
            return csinl (X);
         end;
      end if;
   end Sin;

   function Sinh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function csinhf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, csinhf, "__builtin_csinhf");
         begin
            return csinhf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function csinh (A1 : Complex) return Complex;
            pragma Import (Intrinsic, csinh, "__builtin_csinh");
         begin
            return csinh (X);
         end;
      else
         declare
            function csinhl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, csinhl, "__builtin_csinhl");
         begin
            return csinhl (X);
         end;
      end if;
   end Sinh;

   function Sqrt (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function csqrtf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, csqrtf, "__builtin_csqrtf");
         begin
            return csqrtf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function csqrt (A1 : Complex) return Complex;
            pragma Import (Intrinsic, csqrt, "__builtin_csqrt");
         begin
            return csqrt (X);
         end;
      else
         declare
            function csqrtl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, csqrtl, "__builtin_csqrtl");
         begin
            return csqrtl (X);
         end;
      end if;
   end Sqrt;

   function Tan (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function ctanf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ctanf, "__builtin_ctanf");
         begin
            return ctanf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function ctan (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ctan, "__builtin_ctan");
         begin
            return ctan (X);
         end;
      else
         declare
            function ctanl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ctanl, "__builtin_ctanl");
         begin
            return ctanl (X);
         end;
      end if;
   end Tan;

   function Tanh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function ctanhf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ctanhf, "__builtin_ctanhf");
         begin
            return ctanhf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function ctanh (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ctanh, "__builtin_ctanh");
         begin
            return ctanh (X);
         end;
      else
         declare
            function ctanhl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, ctanhl, "__builtin_ctanhl");
         begin
            return ctanhl (X);
         end;
      end if;
   end Tanh;

   function "**" (Left : Complex; Right : Complex) return Complex is
   begin
      if not Standard'Fast_Math
         and then Left.Re = 0.0 and then Left.Im = 0.0
         and then Right.Re = 0.0 and then Right.Im = 0.0
      then
         raise Argument_Error; -- CXG1004
      elsif Right.Re = 1.0 and Right.Im = 0.0 then
         return Left; -- CXG1005
      elsif Real'Digits <= Float'Digits then
         declare
            function cpowf (A1, A2 : Complex) return Complex;
            pragma Import (Intrinsic, cpowf, "__builtin_cpowf");
         begin
            return cpowf (Left, Right);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function cpow (A1, A2 : Complex) return Complex;
            pragma Import (Intrinsic, cpow, "__builtin_cpow");
         begin
            return cpow (Left, Right);
         end;
      else
         declare
            function cpowl (A1, A2 : Complex) return Complex;
            pragma Import (Intrinsic, cpowl, "__builtin_cpowl");
         begin
            return cpowl (Left, Right);
         end;
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
