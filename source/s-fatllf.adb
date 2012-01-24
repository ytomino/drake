package body System.Fat_LLF is
   pragma Suppress (All_Checks);

   function frexp (value : Long_Long_Float; exp : access Integer)
      return Long_Long_Float;
   pragma Import (Intrinsic, frexp, "__builtin_frexpl");

   function inf return Long_Long_Float;
   pragma Import (Intrinsic, inf, "__builtin_infl");

   function isfinite (X : Long_Long_Float) return Integer;
   pragma Warnings (Off, isfinite); -- [gcc 4.6] excessive prototype checking
   pragma Import (Intrinsic, isfinite, "__builtin_isfinite");

   package body Attr_Long_Long_Float is

      function Compose (Fraction : Long_Long_Float; Exponent : Integer)
         return Long_Long_Float is
      begin
         return Scaling (Attr_Long_Long_Float.Fraction (Fraction), Exponent);
      end Compose;

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

      function Leading_Part (X : Long_Long_Float; Radix_Digits : Integer)
         return Long_Long_Float
      is
         S : constant Integer := Radix_Digits - Exponent (X);
      begin
         return Scaling (Truncation (Scaling (X, S)), -S);
      end Leading_Part;

      function Machine (X : Long_Long_Float) return Long_Long_Float is
      begin
         return X; -- ???
      end Machine;

      function Pred (X : Long_Long_Float) return Long_Long_Float is
      begin
         return Adjacent (X, -inf);
      end Pred;

      function Succ (X : Long_Long_Float) return Long_Long_Float is
      begin
         return Adjacent (X, inf);
      end Succ;

      function Valid (X : not null access Long_Long_Float) return Boolean is
      begin
         return isfinite (X.all) /= 0;
      end Valid;

   end Attr_Long_Long_Float;

end System.Fat_LLF;
