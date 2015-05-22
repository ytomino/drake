pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Fat_Flt is
   pragma Pure;

   package Attr_Float is

      --  required for Float'Adjacent by compiler (s-fatgen.ads)
      function Adjacent (X, Towards : Float) return Float
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_nextafterf";

      --  required for Float'Ceiling by compiler (s-fatgen.ads)
      function Ceiling (X : Float) return Float
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_ceilf";

      --  required for Float'Compose by compiler (s-fatgen.ads)
      function Compose (Fraction : Float; Exponent : Integer) return Float;

      --  required for Float'Copy_Sign by compiler (s-fatgen.ads)
      function Copy_Sign (X, Y : Float) return Float
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_copysignf";

      --  required for Float'Exponent by compiler (s-fatgen.ads)
      function Exponent (X : Float) return Integer;

      --  required for Float'Floor by compiler (s-fatgen.ads)
      function Floor (X : Float) return Float
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_floorf";

      --  required for Float'Fraction by compiler (s-fatgen.ads)
      function Fraction (X : Float) return Float;

      --  required for Float'Leading_Part by compiler (s-fatgen.ads)
      function Leading_Part (X : Float; Radix_Digits : Integer) return Float;

      --  required for Float'Machine by compiler (s-fatgen.ads)
      function Machine (X : Float) return Float;

      --  required for Float'Machine_Rounding by compiler (s-fatgen.ads)
      function Machine_Rounding (X : Float) return Float
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_nearbyintf";

      --  required for Float'Model by compiler (s-fatgen.ads)
      function Model (X : Float) return Float
         renames Machine;

      --  required for Float'Pred by compiler (s-fatgen.ads)
      function Pred (X : Float) return Float;

      --  required for Float'Remainder by compiler (s-fatgen.ads)
      function Remainder (X, Y : Float) return Float
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_remainderf";

      --  required for Float'Rounding by compiler (s-fatgen.ads)
      function Rounding (X : Float) return Float
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_roundf";

      --  required for Float'Scaling by compiler (s-fatgen.ads)
      function Scaling (X : Float; Adjustment : Integer) return Float
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_ldexpf";

      --  required for Float'Succ by compiler (s-fatgen.ads)
      function Succ (X : Float) return Float;

      --  required for Float'Truncation by compiler (s-fatgen.ads)
      function Truncation (X : Float) return Float
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_truncf";

      --  required for Float'Unbiased_Rounding by compiler (s-fatgen.ads)
      function Unbiased_Rounding (X : Float) return Float;

      --  required for Float'Valid by compiler (s-fatgen.ads)
      function Valid (X : not null access Float) return Boolean;
      pragma Export (Ada, Valid, "system__fat_flt__attr_float__valid");
      function Unaligned_Valid (A : Address) return Boolean;
      pragma Import (Ada, Unaligned_Valid,
         "system__fat_flt__attr_float__valid");
      pragma Machine_Attribute (Unaligned_Valid, "pure");

   end Attr_Float;

end System.Fat_Flt;
