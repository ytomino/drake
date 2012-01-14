package body Ada.Float.Elementary_Functions is
   pragma Suppress (All_Checks);

   subtype Float is Standard.Float; -- hiding "Float" package

   --  constants for Sinh/Cosh on high precision mode
   Log_Two : constant := 0.69314_71805_59945_30941_72321_21458_17656_80755;
   Lnv : constant := 8#0.542714#;
   V2minus1 : constant := 0.13830_27787_96019_02638E-4;

   --  implementation

   function Sqrt (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then X < 0.0 then
         raise Argument_Error; -- CXA5A10
      elsif Float_Type'Digits <= Float'Digits then
         declare
            function sqrtf (A1 : Float) return Float;
            pragma Import (Intrinsic, sqrtf, "__builtin_sqrtf");
         begin
            return Float_Type (sqrtf (Float (X)));
         end;
      else
         declare
            function sqrt (A1 : Long_Float) return Long_Float;
            pragma Import (Intrinsic, sqrt, "__builtin_sqrt");
         begin
            return Float_Type (sqrt (Long_Float (X)));
         end;
      end if;
   end Sqrt;

   function Log (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then X < 0.0 then
         raise Argument_Error; -- CXA5A09
      elsif not Standard'Fast_Math and then X = 0.0 then
         raise Constraint_Error; -- CXG2011
      elsif Float_Type'Digits <= Float'Digits then
         declare
            function logf (A1 : Float) return Float;
            pragma Import (Intrinsic, logf, "__builtin_logf");
         begin
            return Float_Type (logf (Float (X)));
         end;
      else
         declare
            function log (A1 : Long_Float) return Long_Float;
            pragma Import (Intrinsic, log, "__builtin_log");
         begin
            return Float_Type (log (Long_Float (X)));
         end;
      end if;
   end Log;

   function Exp (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Float_Type'Digits <= Float'Digits then
         declare
            function expf (A1 : Float) return Float;
            pragma Import (Intrinsic, expf, "__builtin_expf");
         begin
            return Float_Type (expf (Float (X)));
         end;
      else
         declare
            function exp (A1 : Long_Float) return Long_Float;
            pragma Import (Intrinsic, exp, "__builtin_exp");
         begin
            return Float_Type (exp (Long_Float (X)));
         end;
      end if;
   end Exp;

   function Pow (Left, Right : Float_Type'Base) return Float_Type'Base is
      function powf (A1, A2 : Float) return Float;
      pragma Import (Intrinsic, powf, "__builtin_powf");
      function pow (A1, A2 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, pow, "__builtin_pow");
   begin
      if Standard'Fast_Math then
         if Float_Type'Digits <= Float'Digits then
            return Float_Type (powf (Float (Left), Float (Right)));
         else
            return Float_Type (pow (Long_Float (Left), Long_Float (Right)));
         end if;
      else
         if Left < 0.0 or else (Left = 0.0 and then Right = 0.0) then
            raise Argument_Error; -- CXA5A09
         elsif Left = 0.0 and then Right < 0.0 then
            raise Constraint_Error; -- CXG2012
         else
            --  CXG2012 requires high precision
            declare
               function Sqrt is new Elementary_Functions.Sqrt (Float_Type);
               RT : constant Float_Type'Base := Float_Type'Truncation (Right);
               RR : Float_Type'Base;
               Coef : Float_Type'Base;
               Result : Float_Type'Base;
            begin
               if Right - RT = 0.25 then
                  RR := RT;
                  Coef := Sqrt (Sqrt (Left));
               elsif Right - RT = 0.5 then
                  RR := RT;
                  Coef := Sqrt (Left);
               else
                  RR := Right;
                  Coef := 1.0;
               end if;
               if Float_Type'Digits <= Float'Digits then
                  Result := Float_Type (powf (Float (Left), Float (RR)));
               else
                  Result := Float_Type (pow (
                     Long_Float (Left),
                     Long_Float (RR)));
               end if;
               return Result * Coef;
            end;
         end if;
      end if;
   end Pow;

   function Sin (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Float_Type'Digits <= Float'Digits then
         declare
            function sinf (A1 : Float) return Float;
            pragma Import (Intrinsic, sinf, "__builtin_sinf");
         begin
            return Float_Type (sinf (Float (X)));
         end;
      else
         declare
            function sin (A1 : Long_Float) return Long_Float;
            pragma Import (Intrinsic, sin, "__builtin_sin");
         begin
            return Float_Type (sin (Long_Float (X)));
         end;
      end if;
   end Sin;

   function Cos (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Float_Type'Digits <= Float'Digits then
         declare
            function cosf (A1 : Float) return Float;
            pragma Import (Intrinsic, cosf, "__builtin_cosf");
         begin
            return Float_Type (cosf (Float (X)));
         end;
      else
         declare
            function cos (A1 : Long_Float) return Long_Float;
            pragma Import (Intrinsic, cos, "__builtin_cos");
         begin
            return Float_Type (cos (Long_Float (X)));
         end;
      end if;
   end Cos;

   function Tan (X : Float_Type'Base) return Float_Type'Base is
      function tanf (A1 : Float) return Float;
      pragma Import (Intrinsic, tanf, "__builtin_tanf");
      function tan (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, tan, "__builtin_tan");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (tanf (Float (X)));
      else
         return Float_Type (tan (Long_Float (X)));
      end if;
   end Tan;

   function Arcsin (X : Float_Type'Base) return Float_Type'Base is
      function asinf (A1 : Float) return Float;
      pragma Import (Intrinsic, asinf, "__builtin_asinf");
      function asin (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, asin, "__builtin_asin");
   begin
      if not Standard'Fast_Math and then abs X > 1.0 then
         raise Argument_Error; -- CXA5A05
      elsif Float_Type'Digits <= Float'Digits then
         return Float_Type (asinf (Float (X)));
      else
         return Float_Type (asin (Long_Float (X)));
      end if;
   end Arcsin;

   function Arccos (X : Float_Type'Base) return Float_Type'Base is
      function acosf (A1 : Float) return Float;
      pragma Import (Intrinsic, acosf, "__builtin_acosf");
      function acos (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, acos, "__builtin_acos");
   begin
      if not Standard'Fast_Math and then abs X > 1.0 then
         raise Argument_Error; -- CXA5A06
      elsif Float_Type'Digits <= Float'Digits then
         return Float_Type (acosf (Float (X)));
      else
         return Float_Type (acos (Long_Float (X)));
      end if;
   end Arccos;

   function Arctan (Y : Float_Type'Base; X : Float_Type'Base := 1.0)
      return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then X = 0.0 and then Y = 0.0 then
         raise Argument_Error; -- CXA5A07
      elsif Float_Type'Digits <= Float'Digits then
         declare
            function atan2f (A1, A2 : Float) return Float;
            pragma Import (Intrinsic, atan2f, "__builtin_atan2f");
         begin
            return Float_Type (atan2f (Float (Y), Float (X)));
         end;
      else
         declare
            function atan2 (A1, A2 : Long_Float) return Long_Float;
            pragma Import (Intrinsic, atan2, "__builtin_atan2");
         begin
            return Float_Type (atan2 (Long_Float (Y), Long_Float (X)));
         end;
      end if;
   end Arctan;

   function Sinh (X : Float_Type'Base) return Float_Type'Base is
      Log_Inverse_Epsilon : constant Float_Type'Base :=
         Float_Type'Base (Float_Type'Base'Model_Mantissa - 1) * Log_Two;
   begin
      if not Standard'Fast_Math and then abs X > Log_Inverse_Epsilon then
         --  CXG2014 requires high precision
         declare
            function Exp is new Elementary_Functions.Exp (Float_Type);
            Y : constant Float_Type'Base := Exp (abs X - Lnv);
            Z : constant Float_Type'Base := Y + V2minus1 * Y;
         begin
            return Float_Type'Copy_Sign (Z, X);
         end;
      elsif Float_Type'Digits <= Float'Digits then
         declare
            function sinhf (A1 : Float) return Float;
            pragma Import (Intrinsic, sinhf, "__builtin_sinhf");
         begin
            return Float_Type (sinhf (Float (X)));
         end;
      else
         declare
            function sinh (A1 : Long_Float) return Long_Float;
            pragma Import (Intrinsic, sinh, "__builtin_sinh");
         begin
            return Float_Type (sinh (Long_Float (X)));
         end;
      end if;
   end Sinh;

   function Cosh (X : Float_Type'Base) return Float_Type'Base is
      Log_Inverse_Epsilon : constant Float_Type'Base :=
         Float_Type'Base (Float_Type'Base'Model_Mantissa - 1) * Log_Two;
   begin
      if not Standard'Fast_Math and then abs X > Log_Inverse_Epsilon then
         --  CXG2014 requires high precision
         --  graph of Cosh draws catenary line (Cosh (X) = abs Sinh (X))
         declare
            function Exp is new Elementary_Functions.Exp (Float_Type);
            Y : constant Float_Type'Base := Exp (abs X - Lnv);
            Z : constant Float_Type'Base := Y + V2minus1 * Y;
         begin
            return Z;
         end;
      elsif Float_Type'Digits <= Float'Digits then
         declare
            function coshf (A1 : Float) return Float;
            pragma Import (Intrinsic, coshf, "__builtin_coshf");
         begin
            return Float_Type (coshf (Float (X)));
         end;
      else
         declare
            function cosh (A1 : Long_Float) return Long_Float;
            pragma Import (Intrinsic, cosh, "__builtin_cosh");
         begin
            return Float_Type (cosh (Long_Float (X)));
         end;
      end if;
   end Cosh;

   function Tanh (X : Float_Type'Base) return Float_Type'Base is
      function tanhf (A1 : Float) return Float;
      pragma Import (Intrinsic, tanhf, "__builtin_tanhf");
      function tanh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, tanh, "__builtin_tanh");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (tanhf (Float (X)));
      else
         return Float_Type (tanh (Long_Float (X)));
      end if;
   end Tanh;

   function Arcsinh (X : Float_Type'Base) return Float_Type'Base is
      function asinhf (A1 : Float) return Float;
      pragma Import (Intrinsic, asinhf, "__builtin_asinhf");
      function asinh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, asinh, "__builtin_asinh");
   begin
      if Float_Type'Digits <= Float'Digits then
         return Float_Type (asinhf (Float (X)));
      else
         return Float_Type (asinh (Long_Float (X)));
      end if;
   end Arcsinh;

   function Arccosh (X : Float_Type'Base) return Float_Type'Base is
      function acoshf (A1 : Float) return Float;
      pragma Import (Intrinsic, acoshf, "__builtin_acoshf");
      function acosh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, acosh, "__builtin_acosh");
   begin
      if not Standard'Fast_Math and then X < 1.0 then
         raise Argument_Error; -- CXA5A06
      elsif Float_Type'Digits <= Float'Digits then
         return Float_Type (acoshf (Float (X)));
      else
         return Float_Type (acosh (Long_Float (X)));
      end if;
   end Arccosh;

   function Arctanh (X : Float_Type'Base) return Float_Type'Base is
      function atanhf (A1 : Float) return Float;
      pragma Import (Intrinsic, atanhf, "__builtin_atanhf");
      function atanh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, atanh, "__builtin_atanh");
   begin
      if not Standard'Fast_Math and then abs X > 1.0 then
         raise Argument_Error; -- CXA5A03
      elsif Float_Type'Digits <= Float'Digits then
         return Float_Type (atanhf (Float (X)));
      else
         return Float_Type (atanh (Long_Float (X)));
      end if;
   end Arctanh;

end Ada.Float.Elementary_Functions;
