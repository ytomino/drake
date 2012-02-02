pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Elementary_Functions is
   pragma Pure;

   --  Float

   function atan2f (y, x : Float) return Float;
   pragma Import (Intrinsic, atan2f, "__builtin_atan2f");
   function Fast_Arctan (Y : Float; X : Float := 1.0) return Float
      renames atan2f;

   --  Long_Float

   function sqrt (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, sqrt, "__builtin_sqrt");
   function Fast_Sqrt (X : Long_Float) return Long_Float
      renames sqrt;

   function log (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, log, "__builtin_log");
   function Fast_Log (X : Long_Float) return Long_Float
      renames log;

   function exp (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, exp, "__builtin_exp");
   function Fast_Exp (X : Long_Float) return Long_Float
      renames exp;

   function pow (x, y : Long_Float) return Long_Float;
   pragma Import (Intrinsic, pow, "__builtin_pow");
   function Fast_Pow (Left, Right : Long_Float) return Long_Float
      renames pow;

   function sin (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, sin, "__builtin_sin");
   function Fast_Sin (X : Long_Float) return Long_Float
      renames sin;

   function cos (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, cos, "__builtin_cos");
   function Fast_Cos (X : Long_Float) return Long_Float
      renames cos;

   function tan (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, tan, "__builtin_tan");
   function Fast_Tan (X : Long_Float) return Long_Float
      renames tan;

   function asin (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, asin, "__builtin_asin");
   function Fast_Arcsin (X : Long_Float) return Long_Float
      renames asin;

   function acos (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, acos, "__builtin_acos");
   function Fast_Arccos (X : Long_Float) return Long_Float
      renames acos;

   function atan2 (y, x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, atan2, "__builtin_atan2");
   function Fast_Arctan (Y : Long_Float; X : Long_Float := 1.0)
      return Long_Float
      renames atan2;

   function sinh (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, sinh, "__builtin_sinh");
   function Fast_Sinh (X : Long_Float) return Long_Float
      renames sinh;

   function cosh (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, cosh, "__builtin_cosh");
   function Fast_Cosh (X : Long_Float) return Long_Float
      renames cosh;

   function tanh (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, tanh, "__builtin_tanh");
   function Fast_Tanh (X : Long_Float) return Long_Float
      renames tanh;

   function asinh (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, asinh, "__builtin_asinh");
   function Fast_Arcsinh (X : Long_Float) return Long_Float
      renames asinh;

   function acosh (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, acosh, "__builtin_acosh");
   function Fast_Arccosh (X : Long_Float) return Long_Float
      renames acosh;

   function atanh (x : Long_Float) return Long_Float;
   pragma Import (Intrinsic, atanh, "__builtin_atanh");
   function Fast_Arctanh (X : Long_Float) return Long_Float
      renames atanh;

   --  Long_Long_Float

   function Fast_Sqrt (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Sqrt);

   function Fast_Log (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Log);

   function Fast_Exp (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Exp);

   function Fast_Pow (Left, Right : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Pow);

   function Fast_Sin (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Sin);

   function Fast_Cos (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Cos);

   function Fast_Tan (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Tan);

   function Fast_Arcsin (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Arcsin);

   function Fast_Arccos (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Arccos);

   function Fast_Arctan (Y : Long_Long_Float; X : Long_Long_Float := 1.0)
      return Long_Long_Float;
   pragma Inline (Fast_Arctan);

   function Fast_Sinh (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Sinh);

   function Fast_Cosh (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Cosh);

   function Fast_Tanh (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Tanh);

   function Fast_Arcsinh (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Arcsinh);

   function Fast_Arccosh (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Arccosh);

   function Fast_Arctanh (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Arctanh);

end System.Long_Long_Elementary_Functions;
