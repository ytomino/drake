with System.Long_Long_Float_Divide;
package body Ada.Float is
   pragma Suppress (All_Checks);

   function Infinity return Float_Type is
   begin
      if Float_Type'Digits <= Standard.Float'Digits then
         declare
            function inff return Standard.Float;
            pragma Import (Intrinsic, inff, "__builtin_inff");
         begin
            return Float_Type (inff);
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function inf return Long_Float;
            pragma Import (Intrinsic, inf, "__builtin_inf");
         begin
            return Float_Type (inf);
         end;
      else
         declare
            function infl return Long_Long_Float;
            pragma Import (Intrinsic, infl, "__builtin_infl");
         begin
            return Float_Type (infl);
         end;
      end if;
   end Infinity;

   function NaN return Float_Type is
   begin
      if Float_Type'Digits <= Standard.Float'Digits then
         declare
            function nanf (tagp : access constant Character)
               return Standard.Float;
            pragma Import (Intrinsic, nanf, "__builtin_nanf");
            Z : constant array (0 .. 0) of aliased Character :=
               (0 => Character'Val (0));
         begin
            return Float_Type (nanf (Z (0)'Access));
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function nan (tagp : access constant Character) return Long_Float;
            pragma Import (Intrinsic, nan, "__builtin_nan");
            Z : constant array (0 .. 0) of aliased Character :=
               (0 => Character'Val (0));
         begin
            return Float_Type (nan (Z (0)'Access));
         end;
      else
         declare
            function nanl (tagp : access constant Character)
               return Long_Long_Float;
            pragma Import (Intrinsic, nanl, "__builtin_nanl");
            Z : constant array (0 .. 0) of aliased Character :=
               (0 => Character'Val (0));
         begin
            return Float_Type (nanl (Z (0)'Access));
         end;
      end if;
   end NaN;

   function Is_Infinity (X : Float_Type) return Boolean is
   begin
      if Float_Type'Digits <= Standard.Float'Digits then
         declare
            function isinff (x : Standard.Float) return Integer;
            pragma Import (Intrinsic, isinff, "__builtin_isinff");
         begin
            return isinff (Standard.Float (X)) /= 0;
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function isinf (x : Long_Float) return Integer;
            pragma Import (Intrinsic, isinf, "__builtin_isinf");
         begin
            return isinf (Long_Float (X)) /= 0;
         end;
      else
         declare
            function isinfl (x : Long_Long_Float) return Integer;
            pragma Import (Intrinsic, isinfl, "__builtin_isinfl");
         begin
            return isinfl (Long_Long_Float (X)) /= 0;
         end;
      end if;
   end Is_Infinity;

   function Is_NaN (X : Float_Type) return Boolean is
   begin
      if Float_Type'Digits <= Standard.Float'Digits then
         declare
            function isnanf (x : Standard.Float) return Integer;
            pragma Import (Intrinsic, isnanf, "__builtin_isnanf");
         begin
            return isnanf (Standard.Float (X)) /= 0;
         end;
      elsif Float_Type'Digits <= Long_Float'Digits then
         declare
            function isnan (x : Long_Float) return Integer;
            pragma Import (Intrinsic, isnan, "__builtin_isnan");
         begin
            return isnan (Long_Float (X)) /= 0;
         end;
      else
         declare
            function isnanl (x : Long_Long_Float) return Integer;
            pragma Import (Intrinsic, isnanl, "__builtin_isnanl");
         begin
            return isnanl (Long_Long_Float (X)) /= 0;
         end;
      end if;
   end Is_NaN;

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
