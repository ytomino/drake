pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Elementary_Functions is
   pragma Pure;

   function sqrtl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, sqrtl, "__builtin_sqrtl");
   function Fast_Sqrt (X : Long_Long_Float) return Long_Long_Float
      renames sqrtl;

   function logl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, logl, "__builtin_logl");
   function Fast_Log (X : Long_Long_Float) return Long_Long_Float
      renames logl;

   function expl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, expl, "__builtin_expl");
   function Fast_Exp (X : Long_Long_Float) return Long_Long_Float
      renames expl;

   function powl (x, y : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, powl, "__builtin_powl");
   function Fast_Pow (Left, Right : Long_Long_Float) return Long_Long_Float
      renames powl;

   function sinl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, sinl, "__builtin_sinl");
   function Fast_Sin (X : Long_Long_Float) return Long_Long_Float
      renames sinl;

   function cosl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, cosl, "__builtin_cosl");
   function Fast_Cos (X : Long_Long_Float) return Long_Long_Float
      renames cosl;

   function tanl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, tanl, "__builtin_tanl");
   function Fast_Tan (X : Long_Long_Float) return Long_Long_Float
      renames tanl;

   function asinl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, asinl, "__builtin_asinl");
   function Fast_Arcsin (X : Long_Long_Float) return Long_Long_Float
      renames asinl;

   function acosl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, acosl, "__builtin_acosl");
   function Fast_Arccos (X : Long_Long_Float) return Long_Long_Float
      renames acosl;

   function atan2l (y, x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, atan2l, "__builtin_atan2l");
   function Fast_Arctan (Y : Long_Long_Float; X : Long_Long_Float := 1.0)
      return Long_Long_Float
      renames atan2l;

   function sinhl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, sinhl, "__builtin_sinhl");
   function Fast_Sinh (X : Long_Long_Float) return Long_Long_Float
      renames sinhl;

   function coshl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, coshl, "__builtin_coshl");
   function Fast_Cosh (X : Long_Long_Float) return Long_Long_Float
      renames coshl;

   function tanhl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, tanhl, "__builtin_tanhl");
   function Fast_Tanh (X : Long_Long_Float) return Long_Long_Float
      renames tanhl;

   function asinhl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, asinhl, "__builtin_asinhl");
   function Fast_Arcsinh (X : Long_Long_Float) return Long_Long_Float
      renames asinhl;

   function acoshl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, acoshl, "__builtin_acoshl");
   function Fast_Arccosh (X : Long_Long_Float) return Long_Long_Float
      renames acoshl;

   function atanhl (x : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, atanhl, "__builtin_atanhl");
   function Fast_Arctanh (X : Long_Long_Float) return Long_Long_Float
      renames atanhl;

end System.Long_Long_Elementary_Functions;
