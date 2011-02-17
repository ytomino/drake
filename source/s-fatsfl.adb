with System.Long_Long_Float_Divide;
package body System.Fat_Sflt is
   pragma Suppress (All_Checks);

   function frexp (value : Short_Float; exp : access Integer)
      return Short_Float;
   pragma Import (Intrinsic, frexp, "__builtin_frexpf");

   function inf return Short_Float;
   pragma Import (Intrinsic, inf, "__builtin_inff");

   function isfinite (X : Short_Float) return Integer;
   pragma Import (Intrinsic, isfinite, "__builtin_isfinite");

   package body Attr_Short_Float is

      function Compose (Fraction : Short_Float; Exponent : Integer)
         return Short_Float is
      begin
         return Scaling (Attr_Short_Float.Fraction (Fraction), Exponent);
      end Compose;

      function Exponent (X : Short_Float) return Integer is
         Result : aliased Integer;
         Dummy : Short_Float;
         pragma Unreferenced (Dummy);
      begin
         Dummy := frexp (X, Result'Access);
         return Result;
      end Exponent;

      function Fraction (X : Short_Float) return Short_Float is
         Dummy : aliased Integer;
      begin
         return frexp (X, Dummy'Access);
      end Fraction;

      function Leading_Part (X : Short_Float; Radix_Digits : Integer)
         return Short_Float
      is
         S : constant Integer := Radix_Digits - Exponent (X);
      begin
         return Scaling (Truncation (Scaling (X, S)), -S);
      end Leading_Part;

      function Machine (X : Short_Float) return Short_Float is
      begin
         return Short_Float (Long_Long_Float (X)); -- ???
      end Machine;

      function Pred (X : Short_Float) return Short_Float is
      begin
         return Adjacent (X, -inf);
      end Pred;

      function Remainder (X, Y : Short_Float) return Short_Float is
         Q, R : Long_Long_Float;
      begin
         Long_Long_Float_Divide (
            Long_Long_Float (X),
            Long_Long_Float (Y),
            Q,
            R);
         return Short_Float (R);
      end Remainder;

      function Succ (X : Short_Float) return Short_Float is
      begin
         return Adjacent (X, inf);
      end Succ;

      function Unbiased_Rounding (X : Short_Float) return Short_Float is
         Result : Short_Float := Rounding (X);
         Diff : constant Short_Float := Result - X;
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

      function Valid (X : not null access Short_Float) return Boolean is
      begin
         return isfinite (X.all) /= 0;
      end Valid;

   end Attr_Short_Float;

end System.Fat_Sflt;
