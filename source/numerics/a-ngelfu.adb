package body Ada.Numerics.Generic_Elementary_Functions is
   pragma Suppress (All_Checks);

   subtype Float is Standard.Float; -- hiding "Float" package

   --  implementation

   function Arccos (X : Float_Type'Base) return Float_Type'Base is
      function acosf (A1 : Float) return Float;
      pragma Import (Intrinsic, acosf, "__builtin_acosf");
      function acos (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, acos, "__builtin_acos");
      function acosl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, acosl, "__builtin_acosl");
   begin
      if not Standard'Fast_Math and then abs X > 1.0 then
         raise Argument_Error; -- CXA5A06
      elsif Float_Type'Digits <= Float'Digits then
         return Float_Type (acosf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (acos (Long_Float (X)));
      else
         return Float_Type (acosl (Long_Long_Float (X)));
      end if;
   end Arccos;

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Arccos (X) * Cycle / (2.0 * Pi);
      else
         if Cycle <= 0.0 then
            raise Argument_Error; -- CXA5A06
         elsif X = -1.0 then
            return Cycle / 2.0; -- CXG2015
         else
            return Arccos (X) * Cycle / (2.0 * Pi);
         end if;
      end if;
   end Arccos;

   function Arccosh (X : Float_Type'Base) return Float_Type'Base is
      function acoshf (A1 : Float) return Float;
      pragma Import (Intrinsic, acoshf, "__builtin_acoshf");
      function acosh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, acosh, "__builtin_acosh");
      function acoshl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, acoshl, "__builtin_acoshl");
   begin
      if not Standard'Fast_Math and then X < 1.0 then
         raise Argument_Error; -- CXA5A06
      elsif Float_Type'Digits <= Float'Digits then
         return Float_Type (acoshf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (acosh (Long_Float (X)));
      else
         return Float_Type (acoshl (Long_Long_Float (X)));
      end if;
   end Arccosh;

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
      if not Standard'Fast_Math and then Cycle <= 0.0 then
         raise Argument_Error; -- CXA5A08
      else
         return Arccot (X, Y) * Cycle / (2.0 * Pi);
      end if;
   end Arccot;

   function Arccoth (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then abs X < 1.0 then
         raise Argument_Error; -- CXA5A04
      else
         return Log ((X + 1.0) / (X - 1.0)) * 0.5;
      end if;
   end Arccoth;

   function Arcsin (X : Float_Type'Base) return Float_Type'Base is
      function asinf (A1 : Float) return Float;
      pragma Import (Intrinsic, asinf, "__builtin_asinf");
      function asin (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, asin, "__builtin_asin");
      function asinl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, asinl, "__builtin_asinl");
   begin
      if not Standard'Fast_Math and then abs X > 1.0 then
         raise Argument_Error; -- CXA5A05
      elsif Float_Type'Digits <= Float'Digits then
         return Float_Type (asinf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (asin (Long_Float (X)));
      else
         return Float_Type (asinl (Long_Long_Float (X)));
      end if;
   end Arcsin;

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Arcsin (X) * Cycle / (2.0 * Pi);
      else
         if Cycle <= 0.0 then
            raise Argument_Error; -- CXA5A05
         elsif abs X = 1.0 then
            return Float_Type'Base'Copy_Sign (Cycle / 4.0, X); -- CXG2015
         else
            return Arcsin (X) * Cycle / (2.0 * Pi);
         end if;
      end if;
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

   function Arctan (
      Y : Float_Type'Base;
      X : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then Cycle <= 0.0 then
         raise Argument_Error; -- CXA5A07
      elsif not Standard'Fast_Math and then Y = 0.0 then
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

   function Arctanh (X : Float_Type'Base) return Float_Type'Base is
      function atanhf (A1 : Float) return Float;
      pragma Import (Intrinsic, atanhf, "__builtin_atanhf");
      function atanh (A1 : Long_Float) return Long_Float;
      pragma Import (Intrinsic, atanh, "__builtin_atanh");
      function atanhl (A1 : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, atanhl, "__builtin_atanhl");
   begin
      if not Standard'Fast_Math and then abs X > 1.0 then
         raise Argument_Error; -- CXA5A03
      elsif Float_Type'Digits <= Float'Digits then
         return Float_Type (atanhf (Float (X)));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (atanh (Long_Float (X)));
      else
         return Float_Type (atanhl (Long_Long_Float (X)));
      end if;
   end Arctanh;

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Cos (2.0 * Pi * X / Cycle);
      else
         --  CXA5A02 requires just result that is 0.0, 1.0 or -1.0
         --  CXG2004 requires just result that is 0.5
         if Cycle <= 0.0 then
            raise Argument_Error;
         else
            declare
               R : constant Float_Type'Base :=
                  Float_Type'Base'Remainder (X / Cycle, 1.0);
            begin
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
               else
                  return Cos (2.0 * Pi * R);
               end if;
            end;
         end if;
      end if;
   end Cos;

   function Cot (X : Float_Type'Base) return Float_Type'Base is
   begin
      return 1.0 / Tan (X);
   end Cot;

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Cot (2.0 * Pi * X / Cycle);
      else
         if Cycle <= 0.0 then
            raise Argument_Error; -- CXA5A04
         else
            --  CXG2013 requires just result that is 0.0
            declare
               R : constant Float_Type'Base :=
                  Float_Type'Base'Remainder (X / Cycle, 1.0);
            begin
               if R = 0.25 then
                  return 0.0;
               elsif R = 0.75 then
                  return 0.0;
               else
                  return Cot (2.0 * Pi * R);
               end if;
            end;
         end if;
      end if;
   end Cot;

   function Coth (X : Float_Type'Base) return Float_Type'Base is
   begin
      return 1.0 / Tanh (X);
   end Coth;

   function Log (X, Base : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then (Base <= 0.0 or else Base = 1.0) then
         raise Argument_Error; -- CXA5A09
      else
         return Log (X) / Log (Base);
      end if;
   end Log;

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Sin (2.0 * Pi * X / Cycle);
      else
         --  CXA5A01 requires just result that is 0.0, 1.0 or -1.0
         --  CXG2004 requires just result that is 0.5
         if Cycle <= 0.0 then
            raise Argument_Error;
         else
            declare
               R : constant Float_Type'Base :=
                  Float_Type'Base'Remainder (X / Cycle, 1.0);
            begin
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
               else
                  return Sin (2.0 * Pi * R);
               end if;
            end;
         end if;
      end if;
   end Sin;

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
      if Standard'Fast_Math then
         return Tan (2.0 * Pi * X / Cycle);
      else
         if Cycle <= 0.0 then
            raise Argument_Error; -- CXA5A01
         else
            --  CXG2013 requires just result that is 0.0
            declare
               R : constant Float_Type'Base :=
                  Float_Type'Base'Remainder (X / Cycle, 1.0);
            begin
               if R = 0.5 then
                  return 0.0;
               else
                  return Tan (2.0 * Pi * R);
               end if;
            end;
         end if;
      end if;
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

end Ada.Numerics.Generic_Elementary_Functions;
