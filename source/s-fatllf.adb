package body System.Fat_LLF is

   function frexp (value : Long_Long_Float; exp : access Integer)
      return Long_Long_Float;
   pragma Import (Intrinsic, frexp, "__builtin_frexpl");

   function isfinite (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, isfinite, "__builtin_isfinite");

   package body Attr_Long_Long_Float is

      function Exponent (X : Long_Long_Float) return Integer is
         Result : aliased Integer;
         Dummy : Long_Long_Float;
         pragma Unreferenced (Dummy);
      begin
         Dummy := frexp (X, Result'Access);
         return Result;
      end Exponent;

      function Fraction (X : Long_Long_Float) return Long_Long_Float is
         Dummy : aliased Integer;
      begin
         return frexp (X, Dummy'Access);
      end Fraction;

      function Valid (X : not null access Long_Long_Float) return Boolean is
      begin
         return isfinite (X.all) /= 0;
      end Valid;

   end Attr_Long_Long_Float;

end System.Fat_LLF;
