pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Elementary_Functions is
   pragma Pure;

   --  Long_Float

   function log (x : Long_Float) return Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_log";
   function Fast_Log (X : Long_Float) return Long_Float
      renames log;

   function exp (x : Long_Float) return Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_exp";
   function Fast_Exp (X : Long_Float) return Long_Float
      renames exp;

   function pow (x, y : Long_Float) return Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_pow";
   function Fast_Pow (Left, Right : Long_Float) return Long_Float
      renames pow;

   function sinh (x : Long_Float) return Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_sinh";
   function Fast_Sinh (X : Long_Float) return Long_Float
      renames sinh;

   function cosh (x : Long_Float) return Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_cosh";
   function Fast_Cosh (X : Long_Float) return Long_Float
      renames cosh;

   function tanh (x : Long_Float) return Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_tanh";
   function Fast_Tanh (X : Long_Float) return Long_Float
      renames tanh;

   function asinh (x : Long_Float) return Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_asinh";
   function Fast_Arcsinh (X : Long_Float) return Long_Float
      renames asinh;

   function acosh (x : Long_Float) return Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_acosh";
   function Fast_Arccosh (X : Long_Float) return Long_Float
      renames acosh;

   function atanh (x : Long_Float) return Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_atanh";
   function Fast_Arctanh (X : Long_Float) return Long_Float
      renames atanh;

   --  Long_Long_Float

   function Fast_Log (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Log);

   function Fast_Exp (X : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Exp);

   function Fast_Pow (Left, Right : Long_Long_Float) return Long_Long_Float;
   pragma Inline (Fast_Pow);

   function sinl (x : Long_Long_Float) return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_sinl";
   function Fast_Sin (X : Long_Long_Float) return Long_Long_Float
      renames sinl;

   function cosl (x : Long_Long_Float) return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_cosl";
   function Fast_Cos (X : Long_Long_Float) return Long_Long_Float
      renames cosl;

   function atan2l (y, x : Long_Long_Float) return Long_Long_Float
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_atan2l";
   function Fast_Arctan (Y : Long_Long_Float; X : Long_Long_Float := 1.0)
      return Long_Long_Float
      renames atan2l;

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
