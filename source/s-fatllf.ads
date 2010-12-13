pragma License (Unrestricted);
--  implementation package required by compiler
package System.Fat_LLF is
   pragma Pure;

   package Attr_Long_Long_Float is

      --  required for Long_Long_Float'Copy_Sign by compiler (s-fatgen.ads)
      function Copy_Sign (X, Y : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, Copy_Sign, "__builtin_copysignl");

      --  required for Long_Long_Float'Exponent by compiler (s-fatgen.ads)
      function Exponent (X : Long_Long_Float) return Integer;

      --  required for Long_Long_Float'Floor by compiler (s-fatgen.ads)
      function Floor (X : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, Floor, "__builtin_floorl");

      --  required for Long_Long_Float'Fraction by compiler (s-fatgen.ads)
      function Fraction (X : Long_Long_Float) return Long_Long_Float;

      --  required for Long_Long_Float'Rounding by compiler (s-fatgen.ads)
      function Rounding (X : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, Rounding, "__builtin_roundl");

      --  required for Long_Long_Float'Truncation by compiler (s-fatgen.ads)
      function Truncation (X : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, Truncation, "__builtin_truncl");

      --  required for Long_Long_Float'Valid by compiler (s-fatgen.ads)
      function Valid (X : not null access Long_Long_Float) return Boolean;

   end Attr_Long_Long_Float;

end System.Fat_LLF;
