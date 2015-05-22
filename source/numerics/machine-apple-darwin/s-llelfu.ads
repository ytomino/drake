pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Elementary_Functions is
   pragma Pure;

   function logl (x : Long_Long_Float) return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_logl";
   function Fast_Log (X : Long_Long_Float) return Long_Long_Float
      renames logl;

   function expl (x : Long_Long_Float) return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_expl";
   function Fast_Exp (X : Long_Long_Float) return Long_Long_Float
      renames expl;

   function powl (x, y : Long_Long_Float) return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_powl";
   function Fast_Pow (Left, Right : Long_Long_Float) return Long_Long_Float
      renames powl;

   function sinhl (x : Long_Long_Float) return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_sinhl";
   function Fast_Sinh (X : Long_Long_Float) return Long_Long_Float
      renames sinhl;

   function coshl (x : Long_Long_Float) return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_coshl";
   function Fast_Cosh (X : Long_Long_Float) return Long_Long_Float
      renames coshl;

   function tanhl (x : Long_Long_Float) return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_tanhl";
   function Fast_Tanh (X : Long_Long_Float) return Long_Long_Float
      renames tanhl;

   function asinhl (x : Long_Long_Float) return Long_Long_Float
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_asinhl";
   function Fast_Arcsinh (X : Long_Long_Float) return Long_Long_Float
      renames asinhl;

   function acoshl (x : Long_Long_Float) return Long_Long_Float
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_acoshl";
   function Fast_Arccosh (X : Long_Long_Float) return Long_Long_Float
      renames acoshl;

   function atanhl (x : Long_Long_Float) return Long_Long_Float
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_atanhl";
   function Fast_Arctanh (X : Long_Long_Float) return Long_Long_Float
      renames atanhl;

end System.Long_Long_Elementary_Functions;
