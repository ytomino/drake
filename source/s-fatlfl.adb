with System.Long_Long_Float_Divide;
package body System.Fat_Lflt is

   function frexp (value : Long_Float; exp : access Integer)
      return Long_Float;
   pragma Import (Intrinsic, frexp, "__builtin_frexp");

   function inf return Long_Float;
   pragma Import (Intrinsic, inf, "__builtin_inf");

   function isfinite (X : Long_Float) return Integer;
   pragma Import (Intrinsic, isfinite, "__builtin_isfinite");

   package body Attr_Long_Float is

      function Compose (Fraction : Long_Float; Exponent : Integer)
         return Long_Float is
      begin
         return Scaling (Attr_Long_Float.Fraction (Fraction), Exponent);
      end Compose;

      function Exponent (X : Long_Float) return Integer is
         Result : aliased Integer;
         Dummy : Long_Float;
         pragma Unreferenced (Dummy);
      begin
         Dummy := frexp (X, Result'Access);
         return Result;
      end Exponent;

      function Leading_Part (X : Long_Float; Radix_Digits : Integer)
         return Long_Float
      is
         S : constant Integer := Radix_Digits - Exponent (X);
      begin
         return Scaling (Truncation (Scaling (X, S)), -S);
      end Leading_Part;

      function Fraction (X : Long_Float) return Long_Float is
         Dummy : aliased Integer;
      begin
         return frexp (X, Dummy'Access);
      end Fraction;

      function Machine (X : Long_Float) return Long_Float is
      begin
         return Long_Float (Long_Long_Float (X)); -- ???
      end Machine;

      function Pred (X : Long_Float) return Long_Float is
      begin
         return Adjacent (X, -inf);
      end Pred;

      function Remainder (X, Y : Long_Float) return Long_Float is
         Q, R : Long_Long_Float;
      begin
         Long_Long_Float_Divide (
            Long_Long_Float (X),
            Long_Long_Float (Y),
            Q,
            R);
         return Long_Float (R);
      end Remainder;

      function Succ (X : Long_Float) return Long_Float is
      begin
         return Adjacent (X, inf);
      end Succ;

      function Unbiased_Rounding (X : Long_Float) return Long_Float is
         Result : Long_Float := Rounding (X);
         Diff : constant Long_Float := Result - X;
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

      function Valid (X : not null access Long_Float) return Boolean is
      begin
         return isfinite (X.all) /= 0;
      end Valid;

   end Attr_Long_Float;

end System.Fat_Lflt;
