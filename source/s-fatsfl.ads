pragma License (Unrestricted);
--  implementation package required by compiler
package System.Fat_Sflt is
   pragma Pure;

   package Attr_Short_Float is

      --  required for Short_Float'Adjacent by compiler (s-fatgen.ads)
      function Adjacent (X, Towards : Short_Float) return Short_Float;
      pragma Import (Intrinsic, Adjacent, "__builtin_nextafterf");

      --  required for Short_Float'Ceiling by compiler (s-fatgen.ads)
      function Ceiling (X : Short_Float) return Short_Float;
      pragma Import (Intrinsic, Ceiling, "__builtin_ceilf");

      --  required for Short_Float'Compose by compiler (s-fatgen.ads)
      function Compose (Fraction : Short_Float; Exponent : Integer)
         return Short_Float;

      --  required for Short_Float'Copy_Sign by compiler (s-fatgen.ads)
      function Copy_Sign (X, Y : Short_Float) return Float;
      pragma Import (Intrinsic, Copy_Sign, "__builtin_copysignf");

      --  required for Short_Float'Exponent by compiler (s-fatgen.ads)
      function Exponent (X : Short_Float) return Integer;

      --  required for Short_Float'Floor by compiler (s-fatgen.ads)
      function Floor (X : Short_Float) return Short_Float;
      pragma Import (Intrinsic, Floor, "__builtin_floorf");

      --  required for Short_Float'Fraction by compiler (s-fatgen.ads)
      function Fraction (X : Short_Float) return Short_Float;

      --  required for Short_Float'Leading_Part by compiler (s-fatgen.ads)
      function Leading_Part (X : Short_Float; Radix_Digits : Integer)
         return Short_Float;

      --  required for Short_Float'Machine by compiler (s-fatgen.ads)
      function Machine (X : Short_Float) return Short_Float;

      --  required for Short_Float'Machine_Rounding by compiler (s-fatgen.ads)
      function Machine_Rounding (X : Short_Float) return Short_Float;
      pragma Import (Intrinsic, Machine_Rounding, "__builtin_nearbyintf");

      --  required for Short_Float'Model by compiler (s-fatgen.ads)
      function Model (X : Short_Float) return Short_Float
         renames Machine;

      --  required for Short_Float'Pred by compiler (s-fatgen.ads)
      function Pred (X : Short_Float) return Short_Float;

      --  required for Short_Float'Remainder by compiler (s-fatgen.ads)
      function Remainder (X, Y : Short_Float) return Short_Float;

      --  required for Short_Float'Rounding by compiler (s-fatgen.ads)
      function Rounding (X : Short_Float) return Short_Float;
      pragma Import (Intrinsic, Rounding, "__builtin_roundf");

      --  required for Short_Float'Scaling by compiler (s-fatgen.ads)
      function Scaling (X : Short_Float; Adjustment : Integer)
         return Short_Float;
      pragma Import (Intrinsic, Scaling, "__builtin_ldexpf");

      --  required for Short_Float'Succ by compiler (s-fatgen.ads)
      function Succ (X : Short_Float) return Short_Float;

      --  required for Short_Float'Truncation by compiler (s-fatgen.ads)
      function Truncation (X : Short_Float) return Short_Float;
      pragma Import (Intrinsic, Truncation, "__builtin_truncf");

      --  required for Short_Float'Unbiased_Rounding by compiler (s-fatgen.ads)
      function Unbiased_Rounding (X : Short_Float) return Short_Float;

      --  required for Short_Float'Valid by compiler (s-fatgen.ads)
      function Valid (X : not null access Short_Float) return Boolean;
      function Unaligned_Valid (A : System.Address) return Boolean;
      pragma Import (Ada, Unaligned_Valid);
      for Unaligned_Valid'Address use Valid'Address;

   end Attr_Short_Float;

end System.Fat_Sflt;
