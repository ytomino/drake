with System.Long_Long_Float_Divide;
package body Ada.Float is

   function Infinity return Float_Type is
      function inff return Standard.Float;
      pragma Import (Intrinsic, inff, "__builtin_inff");
      function inf return Long_Float;
      pragma Import (Intrinsic, inf, "__builtin_inf");
      function infl return Long_Long_Float;
      pragma Import (Intrinsic, infl, "__builtin_infl");
   begin
      if Float_Type'Digits <= Standard.Float'Digits then
         return Float_Type (inff);
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (inf);
      else
         return Float_Type (infl);
      end if;
   end Infinity;

   function NaN return Float_Type is
      function nanf (tagp : access Character) return Standard.Float;
      pragma Import (Intrinsic, nanf, "__builtin_nanf");
      function nan (tagp : access Character) return Long_Float;
      pragma Import (Intrinsic, nan, "__builtin_nan");
      function nanl (tagp : access Character) return Long_Long_Float;
      pragma Import (Intrinsic, nanl, "__builtin_nanl");
   begin
      if Float_Type'Digits <= Standard.Float'Digits then
         return Float_Type (nanf (null));
      elsif Float_Type'Digits <= Long_Float'Digits then
         return Float_Type (nan (null));
      else
         return Float_Type (nanl (null));
      end if;
   end NaN;

   function Is_Infinity (X : Float_Type) return Boolean is
      function isinff (x : Standard.Float) return Integer;
      pragma Import (Intrinsic, isinff, "__builtin_isinff");
      function isinf (x : Long_Float) return Integer;
      pragma Import (Intrinsic, isinf, "__builtin_isinf");
      function isinfl (x : Long_Long_Float) return Integer;
      pragma Import (Intrinsic, isinfl, "__builtin_isinfl");
   begin
      if Float_Type'Digits <= Standard.Float'Digits then
         return isinff (Standard.Float (X)) /= 0;
      elsif Float_Type'Digits <= Long_Float'Digits then
         return isinf (Long_Float (X)) /= 0;
      else
         return isinfl (Long_Long_Float (X)) /= 0;
      end if;
   end Is_Infinity;

   function Is_NaN (X : Float_Type) return Boolean is
      function isnanf (x : Standard.Float) return Integer;
      pragma Import (Intrinsic, isnanf, "__builtin_isnanf");
      function isnan (x : Long_Float) return Integer;
      pragma Import (Intrinsic, isnan, "__builtin_isnan");
      function isnanl (x : Long_Long_Float) return Integer;
      pragma Import (Intrinsic, isnanl, "__builtin_isnanl");
   begin
      if Float_Type'Digits <= Standard.Float'Digits then
         return isnanf (Standard.Float (X)) /= 0;
      elsif Float_Type'Digits <= Long_Float'Digits then
         return isnan (Long_Float (X)) /= 0;
      else
         return isnanl (Long_Long_Float (X)) /= 0;
      end if;
   end Is_NaN;

   function Is_Negative (X : Float_Type) return Boolean is
      function signbitf (x : Standard.Float) return Integer;
      pragma Import (Intrinsic, signbitf, "__builtin_signbitf");
      function signbit (x : Long_Float) return Integer;
      pragma Import (Intrinsic, signbit, "__builtin_signbit");
      function signbitl (x : Long_Long_Float) return Integer;
      pragma Import (Intrinsic, signbitl, "__builtin_signbitl");
   begin
      if Float_Type'Digits <= Standard.Float'Digits then
         return signbitf (Standard.Float (X)) /= 0;
      elsif Float_Type'Digits <= Long_Float'Digits then
         return signbit (Long_Float (X)) /= 0;
      else
         return signbitl (Long_Long_Float (X)) /= 0;
      end if;
   end Is_Negative;

   procedure Divide (
      Dividend : Dividend_Type;
      Divisor : Divisor_Type;
      Quotient : out Quotient_Type;
      Remainder : out Remainder_Type) is
   begin
      System.Long_Long_Float_Divide (
         Long_Long_Float (Dividend),
         Long_Long_Float (Divisor),
         Long_Long_Float (Quotient),
         Long_Long_Float (Remainder));
   end Divide;

end Ada.Float;
