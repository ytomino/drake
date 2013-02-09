pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Fat_Flt is
   pragma Pure;

   package Attr_Float is

      --  required for Float'Adjacent by compiler (s-fatgen.ads)
      function Adjacent (X, Towards : Float) return Float;
      pragma Import (Intrinsic, Adjacent, "__builtin_nextafterf");

      --  required for Float'Ceiling by compiler (s-fatgen.ads)
      function Ceiling (X : Float) return Float;
      pragma Import (Intrinsic, Ceiling, "__builtin_ceilf");

      --  required for Float'Compose by compiler (s-fatgen.ads)
      function Compose (Fraction : Float; Exponent : Integer) return Float;

      --  required for Float'Copy_Sign by compiler (s-fatgen.ads)
      function Copy_Sign (X, Y : Float) return Float;
      pragma Import (Intrinsic, Copy_Sign, "__builtin_copysignf");

      --  required for Float'Exponent by compiler (s-fatgen.ads)
      function Exponent (X : Float) return Integer;

      --  required for Float'Floor by compiler (s-fatgen.ads)
      function Floor (X : Float) return Float;
      pragma Import (Intrinsic, Floor, "__builtin_floorf");

      --  required for Float'Fraction by compiler (s-fatgen.ads)
      function Fraction (X : Float) return Float;

      --  required for Float'Leading_Part by compiler (s-fatgen.ads)
      function Leading_Part (X : Float; Radix_Digits : Integer) return Float;

      --  required for Float'Machine by compiler (s-fatgen.ads)
      function Machine (X : Float) return Float;

      --  required for Float'Machine_Rounding by compiler (s-fatgen.ads)
      function Machine_Rounding (X : Float) return Float;
      pragma Import (Intrinsic, Machine_Rounding, "__builtin_nearbyintf");

      --  required for Float'Model by compiler (s-fatgen.ads)
      function Model (X : Float) return Float
         renames Machine;

      --  required for Float'Pred by compiler (s-fatgen.ads)
      function Pred (X : Float) return Float;

      --  required for Float'Remainder by compiler (s-fatgen.ads)
      function Remainder (X, Y : Float) return Float;

      --  required for Float'Rounding by compiler (s-fatgen.ads)
      function Rounding (X : Float) return Float;
      pragma Import (Intrinsic, Rounding, "__builtin_roundf");

      --  required for Float'Scaling by compiler (s-fatgen.ads)
      function Scaling (X : Float; Adjustment : Integer) return Float;
      pragma Import (Intrinsic, Scaling, "__builtin_ldexpf");

      --  required for Float'Succ by compiler (s-fatgen.ads)
      function Succ (X : Float) return Float;

      --  required for Float'Truncation by compiler (s-fatgen.ads)
      function Truncation (X : Float) return Float;
      pragma Import (Intrinsic, Truncation, "__builtin_truncf");

      --  required for Float'Unbiased_Rounding by compiler (s-fatgen.ads)
      function Unbiased_Rounding (X : Float) return Float;

      --  required for Float'Valid by compiler (s-fatgen.ads)
      function Valid (X : not null access Float) return Boolean;
      function Unaligned_Valid (A : Address) return Boolean;
      pragma Import (Ada, Unaligned_Valid);
      pragma Pure_Function (Unaligned_Valid);
      for Unaligned_Valid'Address use Valid'Address;

   end Attr_Float;

end System.Fat_Flt;
