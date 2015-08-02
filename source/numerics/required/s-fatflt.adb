package body System.Fat_Flt is

   function frexp (value : Float; exp : access Integer)
      return Float
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_frexpf";

   function inf return Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_inff";

   function isfinite (X : Float) return Integer
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_isfinite";
   pragma Warnings (Off, isfinite); -- [gcc 4.6] excessive prototype checking

   package body Attr_Float is

      function Compose (Fraction : Float; Exponent : Integer) return Float is
      begin
         return Scaling (Attr_Float.Fraction (Fraction), Exponent);
      end Compose;

      function Exponent (X : Float) return Integer is
         Result : aliased Integer;
         Dummy : Float;
         pragma Unreferenced (Dummy);
      begin
         Dummy := frexp (X, Result'Access);
         return Result;
      end Exponent;

      function Fraction (X : Float) return Float is
         Dummy : aliased Integer;
      begin
         return frexp (X, Dummy'Access);
      end Fraction;

      function Leading_Part (X : Float; Radix_Digits : Integer) return Float is
         S : constant Integer := Radix_Digits - Exponent (X);
      begin
         return Scaling (Truncation (Scaling (X, S)), -S);
      end Leading_Part;

      function Pred (X : Float) return Float is
      begin
         return Adjacent (X, -inf);
      end Pred;

      function Machine (X : Float) return Float is
      begin
         return Float (Long_Long_Float (X)); -- ???
      end Machine;

      function Succ (X : Float) return Float is
      begin
         return Adjacent (X, inf);
      end Succ;

      function Unbiased_Rounding (X : Float) return Float is
      begin
         return X - Remainder (X, 1.0);
      end Unbiased_Rounding;

      function Valid (X : not null access Float) return Boolean is
      begin
         return isfinite (X.all) /= 0;
      end Valid;

   end Attr_Float;

end System.Fat_Flt;
