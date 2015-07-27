with Ada.Float;
with System.Long_Long_Elementary_Functions;
package body Ada.Numerics.Generic_Elementary_Functions is
   pragma Suppress (All_Checks);

   procedure Modulo_Divide_By_1 is
      new Float.Modulo_Divide_By_1 (
         Float_Type'Base,
         Float_Type'Base,
         Float_Type'Base);
   subtype Float is Standard.Float; -- hiding "Float" package

   --  constants for Sinh/Cosh on high precision mode
   Log_Two : constant := 0.69314_71805_59945_30941_72321_21458_17656_80755;
   Lnv : constant := 8#0.542714#;
   V2minus1 : constant := 0.13830_27787_96019_02638E-4;

   --  implementation

   function Sqrt (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (X >= 0.0) then
         raise Argument_Error; -- CXA5A10
      end if;
      if Float_Type'Digits <= Float'Digits then
         declare
            function sqrtf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_sqrtf";
         begin
            return Float_Type'Base (sqrtf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function sqrt (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_sqrt";
         begin
            return Float_Type'Base (sqrt (Long_Float (X)));
         end;
      else
         declare
            function sqrtl (x : Long_Long_Float) return Long_Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_sqrtl";
         begin
            return Float_Type'Base (sqrtl (Long_Long_Float (X)));
         end;
      end if;
   end Sqrt;

   function Log (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (X > 0.0) then
         if X = 0.0 then
            raise Constraint_Error; -- CXG2011
         else
            raise Argument_Error; -- CXA5A09
         end if;
      end if;
      if Float_Type'Digits <= Float'Digits then
         declare
            function logf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_logf";
         begin
            return Float_Type'Base (logf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function log (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_log";
         begin
            return Float_Type'Base (log (Long_Float (X)));
         end;
      else
         return Float_Type'Base (
            System.Long_Long_Elementary_Functions.Fast_Log (
               Long_Long_Float (X)));
      end if;
   end Log;

   function Log (X, Base : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math
         and then not (Base > 0.0 and then Base /= 1.0)
      then
         raise Argument_Error; -- CXA5A09
      end if;
      return Log (X) / Log (Base);
   end Log;

   function Exp (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Float_Type'Digits <= Float'Digits then
         declare
            function expf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_expf";
         begin
            return Float_Type'Base (expf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function exp (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_exp";
         begin
            return Float_Type'Base (exp (Long_Float (X)));
         end;
      else
         return Float_Type'Base (
            System.Long_Long_Elementary_Functions.Fast_Exp (
               Long_Long_Float (X)));
      end if;
   end Exp;

   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base is
      RR : Float_Type'Base;
      Coef : Float_Type'Base;
      Result : Float_Type'Base;
   begin
      if not Standard'Fast_Math then
         if not (Left > 0.0 or else (Left = 0.0 and then Right > 0.0)) then
            if Left = 0.0 and then Right < 0.0 then
               raise Constraint_Error; -- CXG2012
            else
               raise Argument_Error; -- CXA5A09
            end if;
         end if;
         --  CXG2012 requires high precision
         declare
            RT : constant Float_Type'Base := Float_Type'Truncation (Right);
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
         end;
      else
         RR := Right;
         Coef := 1.0;
      end if;
      if Float_Type'Digits <= Float'Digits then
         declare
            function powf (A1, A2 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_powf";
         begin
            Result := Float_Type'Base (powf (Float (Left), Float (RR)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function pow (A1, A2 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_pow";
         begin
            Result := Float_Type'Base (
               pow (Long_Float (Left), Long_Float (RR)));
         end;
      else
         Result := Float_Type'Base (
            System.Long_Long_Elementary_Functions.Fast_Pow (
               Long_Long_Float (Left),
               Long_Long_Float (RR)));
      end if;
      return Result * Coef;
   end "**";

   function Sin (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Float_Type'Digits <= Float'Digits then
         declare
            function sinf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_sinf";
         begin
            return Float_Type'Base (sinf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function sin (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_sin";
         begin
            return Float_Type'Base (sin (Long_Float (X)));
         end;
      else
         declare
            function sinl (x : Long_Long_Float) return Long_Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_sinl";
         begin
            return Float_Type'Base (sinl (Long_Long_Float (X)));
         end;
      end if;
   end Sin;

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base is
      R : Float_Type'Base;
   begin
      if not Standard'Fast_Math then
         if not (Cycle > 0.0) then
            raise Argument_Error;
         end if;
         --  CXA5A01 requires just result that is 0.0, 1.0 or -1.0
         --  CXG2004 requires just result that is 0.5
         declare
            Q : Float_Type'Base;
         begin
            Modulo_Divide_By_1 (X / Cycle, Q, R);
            if R = 1.0 / 12.0 then
               return 0.5;
            elsif R = 0.25 then
               return 1.0;
            elsif R = 5.0 / 12.0 then
               return 0.5;
            elsif R = 0.5 then
               return 0.0;
            elsif R = 7.0 / 12.0 then
               return -0.5;
            elsif R = 0.75 then
               return -1.0;
            elsif R = 11.0 / 12.0 then
               return -0.5;
            end if;
         end;
      else
         R := X / Cycle;
      end if;
      return Sin (2.0 * Pi * R);
   end Sin;

   function Cos (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Float_Type'Digits <= Float'Digits then
         declare
            function cosf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_cosf";
         begin
            return Float_Type'Base (cosf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function cos (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_cos";
         begin
            return Float_Type'Base (cos (Long_Float (X)));
         end;
      else
         declare
            function cosl (x : Long_Long_Float) return Long_Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_cosl";
         begin
            return Float_Type'Base (cosl (Long_Long_Float (X)));
         end;
      end if;
   end Cos;

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base is
      R : Float_Type'Base;
   begin
      if not Standard'Fast_Math then
         if not (Cycle > 0.0) then
            raise Argument_Error;
         end if;
         --  CXA5A02 requires just result that is 0.0, 1.0 or -1.0
         --  CXG2004 requires just result that is 0.5
         declare
            Q : Float_Type'Base;
         begin
            Modulo_Divide_By_1 (X / Cycle, Q, R);
            if R = 2.0 / 12.0 then
               return 0.5;
            elsif R = 0.25 then
               return 0.0;
            elsif R = 4.0 / 12.0 then
               return -0.5;
            elsif R = 0.5 then
               return -1.0;
            elsif R = 8.0 / 12.0 then
               return -0.5;
            elsif R = 0.75 then
               return 0.0;
            elsif R = 10.0 / 12.0 then
               return 0.5;
            end if;
         end;
      else
         R := X / Cycle;
      end if;
      return Cos (2.0 * Pi * R);
   end Cos;

   function Tan (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Float_Type'Digits <= Float'Digits then
         declare
            function tanf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_tanf";
         begin
            return Float_Type'Base (tanf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function tan (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_tan";
         begin
            return Float_Type'Base (tan (Long_Float (X)));
         end;
      else
         declare
            function tanl (x : Long_Long_Float) return Long_Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_tanl";
         begin
            return Float_Type'Base (tanl (Long_Long_Float (X)));
         end;
      end if;
   end Tan;

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base is
      R : Float_Type'Base;
   begin
      if not Standard'Fast_Math then
         if not (Cycle > 0.0) then
            raise Argument_Error; -- CXA5A01
         end if;
         --  CXG2013 requires just result that is 0.0
         declare
            Q : Float_Type'Base;
         begin
            Modulo_Divide_By_1 (X / Cycle, Q, R);
            if R = 0.5 then
               return 0.0;
            end if;
         end;
      else
         R := X / Cycle;
      end if;
      return Tan (2.0 * Pi * R);
   end Tan;

   function Cot (X : Float_Type'Base) return Float_Type'Base is
   begin
      return 1.0 / Tan (X);
   end Cot;

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base is
      R : Float_Type'Base;
   begin
      if not Standard'Fast_Math then
         if not (Cycle > 0.0) then
            raise Argument_Error; -- CXA5A04
         end if;
         --  CXG2013 requires just result that is 0.0
         declare
            Q : Float_Type'Base;
         begin
            Modulo_Divide_By_1 (X / Cycle, Q, R);
            if R = 0.25 then
               return 0.0;
            elsif R = 0.75 then
               return 0.0;
            end if;
         end;
      else
         R := X / Cycle;
      end if;
      return Cot (2.0 * Pi * R);
   end Cot;

   function Arcsin (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (abs X <= 1.0) then
         raise Argument_Error; -- CXA5A05
      end if;
      if Float_Type'Digits <= Float'Digits then
         declare
            function asinf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_asinf";
         begin
            return Float_Type'Base (asinf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function asin (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_asin";
         begin
            return Float_Type'Base (asin (Long_Float (X)));
         end;
      else
         declare
            function asinl (x : Long_Long_Float) return Long_Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_asinl";
         begin
            return Float_Type'Base (asinl (Long_Long_Float (X)));
         end;
      end if;
   end Arcsin;

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math then
         if not (Cycle > 0.0) then
            raise Argument_Error; -- CXA5A05
         end if;
         --  CXG2015 requires
         if abs X = 1.0 then
            return Float_Type'Base'Copy_Sign (Cycle / 4.0, X);
         end if;
      end if;
      return Arcsin (X) * Cycle / (2.0 * Pi);
   end Arcsin;

   function Arccos (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (abs X <= 1.0) then
         raise Argument_Error; -- CXA5A06
      end if;
      if Float_Type'Digits <= Float'Digits then
         declare
            function acosf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_acosf";
         begin
            return Float_Type'Base (acosf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function acos (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_acos";
         begin
            return Float_Type'Base (acos (Long_Float (X)));
         end;
      else
         declare
            function acosl (x : Long_Long_Float) return Long_Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_acosl";
         begin
            return Float_Type'Base (acosl (Long_Long_Float (X)));
         end;
      end if;
   end Arccos;

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math then
         if not (Cycle > 0.0) then
            raise Argument_Error; -- CXA5A06
         end if;
         --  CXG2015 requires
         if X = -1.0 then
            return Cycle / 2.0;
         end if;
      end if;
      return Arccos (X) * Cycle / (2.0 * Pi);
   end Arccos;

   function Arctan (Y : Float_Type'Base; X : Float_Type'Base := 1.0)
      return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (X /= 0.0 or else Y /= 0.0) then
         raise Argument_Error; -- CXA5A07
      end if;
      if Float_Type'Digits <= Float'Digits then
         declare
            function atan2f (A1, A2 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_atan2f";
         begin
            return Float_Type'Base (atan2f (Float (Y), Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function atan2 (A1, A2 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_atan2";
         begin
            return Float_Type'Base (atan2 (Long_Float (Y), Long_Float (X)));
         end;
      else
         declare
            function atan2l (y, x : Long_Long_Float) return Long_Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_atan2l";
         begin
            return Float_Type'Base (
               atan2l (Long_Long_Float (Y), Long_Long_Float (X)));
         end;
      end if;
   end Arctan;

   function Arctan (
      Y : Float_Type'Base;
      X : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (Cycle > 0.0) then
         raise Argument_Error; -- CXA5A07
      end if;
      if not Standard'Fast_Math and then Y = 0.0 then
         --  CXG2016 requires
         if X < 0.0 then
            return Cycle / 2.0 * Float_Type'Copy_Sign (1.0, Y);
         else
            return 0.0;
         end if;
      else
         return Arctan (Y, X) * Cycle / (2.0 * Pi);
      end if;
   end Arctan;

   function Arccot (X : Float_Type'Base; Y : Float_Type'Base := 1.0)
      return Float_Type'Base is
   begin
      return Arctan (Y, X);
   end Arccot;

   function Arccot (
      X : Float_Type'Base;
      Y : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (Cycle > 0.0) then
         raise Argument_Error; -- CXA5A08
      end if;
      return Arccot (X, Y) * Cycle / (2.0 * Pi);
   end Arccot;

   function Sinh (X : Float_Type'Base) return Float_Type'Base is
      Log_Inverse_Epsilon : constant Float_Type'Base :=
         Float_Type'Base (Float_Type'Base'Model_Mantissa - 1) * Log_Two;
   begin
      if not Standard'Fast_Math and then abs X > Log_Inverse_Epsilon then
         --  CXG2014 requires high precision
         declare
            Y : constant Float_Type'Base := Exp (abs X - Lnv);
            Z : constant Float_Type'Base := Y + V2minus1 * Y;
         begin
            return Float_Type'Copy_Sign (Z, X);
         end;
      else
         if Float_Type'Digits <= Float'Digits then
            declare
               function sinhf (A1 : Float) return Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_sinhf";
            begin
               return Float_Type'Base (sinhf (Float (X)));
            end;
         elsif Float_Type'Digits <= Long_Float'Digits then
            declare
               function sinh (A1 : Long_Float) return Long_Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_sinh";
            begin
               return Float_Type'Base (sinh (Long_Float (X)));
            end;
         else
            return Float_Type'Base (
               System.Long_Long_Elementary_Functions.Fast_Sinh (
                  Long_Long_Float (X)));
         end if;
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
            Y : constant Float_Type'Base := Exp (abs X - Lnv);
            Z : constant Float_Type'Base := Y + V2minus1 * Y;
         begin
            return Z;
         end;
      else
         if Float_Type'Digits <= Float'Digits then
            declare
               function coshf (A1 : Float) return Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_coshf";
            begin
               return Float_Type'Base (coshf (Float (X)));
            end;
         elsif Float_Type'Digits <= Long_Float'Digits then
            declare
               function cosh (A1 : Long_Float) return Long_Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_cosh";
            begin
               return Float_Type'Base (cosh (Long_Float (X)));
            end;
         else
            return Float_Type'Base (
               System.Long_Long_Elementary_Functions.Fast_Cosh (
                  Long_Long_Float (X)));
         end if;
      end if;
   end Cosh;

   function Tanh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Float_Type'Digits <= Float'Digits then
         declare
            function tanhf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_tanhf";
         begin
            return Float_Type'Base (tanhf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function tanh (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_tanh";
         begin
            return Float_Type'Base (tanh (Long_Float (X)));
         end;
      else
         return Float_Type'Base (
            System.Long_Long_Elementary_Functions.Fast_Tanh (
               Long_Long_Float (X)));
      end if;
   end Tanh;

   function Coth (X : Float_Type'Base) return Float_Type'Base is
   begin
      return 1.0 / Tanh (X);
   end Coth;

   function Arcsinh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Float_Type'Digits <= Float'Digits then
         declare
            function asinhf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_asinhf";
         begin
            return Float_Type'Base (asinhf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function asinh (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_asinh";
         begin
            return Float_Type'Base (asinh (Long_Float (X)));
         end;
      else
         return Float_Type'Base (
            System.Long_Long_Elementary_Functions.Fast_Arcsinh (
               Long_Long_Float (X)));
      end if;
   end Arcsinh;

   function Arccosh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (X >= 1.0) then
         raise Argument_Error; -- CXA5A06
      end if;
      if Float_Type'Digits <= Float'Digits then
         declare
            function acoshf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_acoshf";
         begin
            return Float_Type'Base (acoshf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function acosh (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_acosh";
         begin
            return Float_Type'Base (acosh (Long_Float (X)));
         end;
      else
         return Float_Type'Base (
            System.Long_Long_Elementary_Functions.Fast_Arccosh (
               Long_Long_Float (X)));
      end if;
   end Arccosh;

   function Arctanh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (abs X <= 1.0) then
         raise Argument_Error; -- CXA5A03
      end if;
      if Float_Type'Digits <= Float'Digits then
         declare
            function atanhf (A1 : Float) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_atanhf";
         begin
            return Float_Type'Base (atanhf (Float (X)));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function atanh (A1 : Long_Float) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_atanh";
         begin
            return Float_Type'Base (atanh (Long_Float (X)));
         end;
      else
         return Float_Type'Base (
            System.Long_Long_Elementary_Functions.Fast_Arctanh (
               Long_Long_Float (X)));
      end if;
   end Arctanh;

   function Arccoth (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then not (abs X >= 1.0) then
         raise Argument_Error; -- CXA5A04
      end if;
      return Log ((X + 1.0) / (X - 1.0)) * 0.5;
   end Arccoth;

end Ada.Numerics.Generic_Elementary_Functions;
