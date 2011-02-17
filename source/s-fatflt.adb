with System.Long_Long_Float_Divide;
package body System.Fat_Flt is

   function frexp (value : Float; exp : access Integer)
      return Float;
   pragma Import (Intrinsic, frexp, "__builtin_frexpf");

   function inf return Float;
   pragma Import (Intrinsic, inf, "__builtin_inff");

   function isfinite (X : Float) return Integer;
   pragma Import (Intrinsic, isfinite, "__builtin_isfinite");

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

      function Remainder (X, Y : Float) return Float is
         Q, R : Long_Long_Float;
      begin
         Long_Long_Float_Divide (
            Long_Long_Float (X),
            Long_Long_Float (Y),
            Q,
            R);
         return Float (R);
      end Remainder;

      function Succ (X : Float) return Float is
      begin
         return Adjacent (X, inf);
      end Succ;

      function Unbiased_Rounding (X : Float) return Float is
         Result : Float := Rounding (X);
         Diff : constant Float := Result - X;
      begin
         if Diff = 0.5 then
            --  1.5 -> 2.0, 2.5 -> 3.0, ...
            if Truncation (Result / 2.0) * 2.0 /= Result then
               Result := Result - 1.0;
            end if;
         elsif Diff = -0.5 then
            --  -1.5 -> -2.0, -2.5 -> -3.0, ...
            if Truncation (Result / 2.0) * 2.0 /= Result then
               Result := Result + 1.0;
            end if;
         end if;
         return Result;
      end Unbiased_Rounding;

      function Valid (X : not null access Float) return Boolean is
      begin
         return isfinite (X.all) /= 0;
      end Valid;

   end Attr_Float;

end System.Fat_Flt;
