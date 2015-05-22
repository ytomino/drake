pragma License (Unrestricted);
--  implementation unit
with System.Long_Long_Complex_Types;
package System.Long_Long_Complex_Elementary_Functions is
   pragma Pure;

   --  Complex

   subtype Imaginary is Long_Long_Complex_Types.Imaginary;
   subtype Complex is Long_Long_Complex_Types.Complex;

   function clogf (x : Complex) return Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_clogf";
   function Fast_Log (X : Complex) return Complex
      renames clogf;

   function cexpf (x : Complex) return Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_cexpf";
   function Fast_Exp (X : Complex) return Complex
      renames cexpf;

   function cexpif (x : Imaginary) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_cexpif";
   function Fast_Exp (X : Imaginary) return Complex
      renames cexpif;

   function cpowf (x, y : Complex) return Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_cpowf";
   function Fast_Pow (Left, Right : Complex) return Complex
      renames cpowf;

   function csinf (x : Complex) return Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_csinf";
   function Fast_Sin (X : Complex) return Complex
      renames csinf;

   function ccosf (x : Complex) return Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_ccosf";
   function Fast_Cos (X : Complex) return Complex
      renames ccosf;

   function ctanf (x : Complex) return Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_ctanf";
   function Fast_Tan (X : Complex) return Complex
      renames ctanf;

   function casinf (x : Complex) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_casinf";
   function Fast_Arcsin (X : Complex) return Complex
      renames casinf;

   function cacosf (x : Complex) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_cacosf";
   function Fast_Arccos (X : Complex) return Complex
      renames cacosf;

   function catanf (x : Complex) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_catanf";
   function Fast_Arctan (X : Complex) return Complex
      renames catanf;

   function csinhf (x : Complex) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_csinhf";
   function Fast_Sinh (X : Complex) return Complex
      renames csinhf;

   function ccoshf (x : Complex) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ccoshf";
   function Fast_Cosh (X : Complex) return Complex
      renames ccoshf;

   function ctanhf (x : Complex) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ctanhf";
   function Fast_Tanh (X : Complex) return Complex
      renames ctanhf;

   function casinhf (x : Complex) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_casinhf";
   function Fast_Arcsinh (X : Complex) return Complex
      renames casinhf;

   function cacoshf (x : Complex) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_cacoshf";
   function Fast_Arccosh (X : Complex) return Complex
      renames cacoshf;

   function catanhf (x : Complex) return Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_catanhf";
   function Fast_Arctanh (X : Complex) return Complex
      renames catanhf;

   --  Long_Complex

   subtype Long_Imaginary is Long_Long_Complex_Types.Long_Imaginary;
   subtype Long_Complex is Long_Long_Complex_Types.Long_Complex;

   function clog (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_clog";
   function Fast_Log (X : Long_Complex) return Long_Complex
      renames clog;

   function cexp (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_cexp";
   function Fast_Exp (X : Long_Complex) return Long_Complex
      renames cexp;

   function cexpi (x : Long_Imaginary) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_cexpi";
   function Fast_Exp (X : Long_Imaginary) return Long_Complex
      renames cexpi;

   function cpow (x, y : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_cpow";
   function Fast_Pow (Left, Right : Long_Complex) return Long_Complex
      renames cpow;

   function csin (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_csin";
   function Fast_Sin (X : Long_Complex) return Long_Complex
      renames csin;

   function ccos (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_ccos";
   function Fast_Cos (X : Long_Complex) return Long_Complex
      renames ccos;

   function ctan (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_ctan";
   function Fast_Tan (X : Long_Complex) return Long_Complex
      renames ctan;

   function casin (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_casin";
   function Fast_Arcsin (X : Long_Complex) return Long_Complex
      renames casin;

   function cacos (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_cacos";
   function Fast_Arccos (X : Long_Complex) return Long_Complex
      renames cacos;

   function catan (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_catan";
   function Fast_Arctan (X : Long_Complex) return Long_Complex
      renames catan;

   function csinh (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_csinh";
   function Fast_Sinh (X : Long_Complex) return Long_Complex
      renames csinh;

   function ccosh (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_ccosh";
   function Fast_Cosh (X : Long_Complex) return Long_Complex
      renames ccosh;

   function ctanh (x : Long_Complex) return Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_ctanh";
   function Fast_Tanh (X : Long_Complex) return Long_Complex
      renames ctanh;

   function casinh (x : Long_Complex) return Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_casinh";
   function Fast_Arcsinh (X : Long_Complex) return Long_Complex
      renames casinh;

   function cacosh (x : Long_Complex) return Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_cacosh";
   function Fast_Arccosh (X : Long_Complex) return Long_Complex
      renames cacosh;

   function catanh (x : Long_Complex) return Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_catanh";
   function Fast_Arctanh (X : Long_Complex) return Long_Complex
      renames catanh;

   --  Long_Long_Complex

   subtype Long_Long_Imaginary is Long_Long_Complex_Types.Long_Long_Imaginary;
   subtype Long_Long_Complex is Long_Long_Complex_Types.Long_Long_Complex;

   function clogl (x : Long_Long_Complex) return Long_Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_clogl";
   function Fast_Log (X : Long_Long_Complex) return Long_Long_Complex
      renames clogl;

   function cexpl (x : Long_Long_Complex) return Long_Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_cexpl";
   function Fast_Exp (X : Long_Long_Complex) return Long_Long_Complex
      renames cexpl;

   function cexpil (x : Long_Long_Imaginary) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_cexpil";
   function Fast_Exp (X : Long_Long_Imaginary) return Long_Long_Complex
      renames cexpil;

   function cpowl (x, y : Long_Long_Complex) return Long_Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_cpowl";
   function Fast_Pow (Left, Right : Long_Long_Complex) return Long_Long_Complex
      renames cpowl;

   function csinl (x : Long_Long_Complex) return Long_Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_csinl";
   function Fast_Sin (X : Long_Long_Complex) return Long_Long_Complex
      renames csinl;

   function ccosl (x : Long_Long_Complex) return Long_Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_ccosl";
   function Fast_Cos (X : Long_Long_Complex) return Long_Long_Complex
      renames ccosl;

   function ctanl (x : Long_Long_Complex) return Long_Long_Complex
      with Import, Convention => Intrinsic, External_Name => "__builtin_ctanl";
   function Fast_Tan (X : Long_Long_Complex) return Long_Long_Complex
      renames ctanl;

   function casinl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_casinl";
   function Fast_Arcsin (X : Long_Long_Complex) return Long_Long_Complex
      renames casinl;

   function cacosl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_cacosl";
   function Fast_Arccos (X : Long_Long_Complex) return Long_Long_Complex
      renames cacosl;

   function catanl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_catanl";
   function Fast_Arctan (X : Long_Long_Complex) return Long_Long_Complex
      renames catanl;

   function csinhl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_csinhl";
   function Fast_Sinh (X : Long_Long_Complex) return Long_Long_Complex
      renames csinhl;

   function ccoshl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ccoshl";
   function Fast_Cosh (X : Long_Long_Complex) return Long_Long_Complex
      renames ccoshl;

   function ctanhl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ctanhl";
   function Fast_Tanh (X : Long_Long_Complex) return Long_Long_Complex
      renames ctanhl;

   function casinhl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_casinhl";
   function Fast_Arcsinh (X : Long_Long_Complex) return Long_Long_Complex
      renames casinhl;

   function cacoshl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_cacoshl";
   function Fast_Arccosh (X : Long_Long_Complex) return Long_Long_Complex
      renames cacoshl;

   function catanhl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_catanhl";
   function Fast_Arctanh (X : Long_Long_Complex) return Long_Long_Complex
      renames catanhl;

end System.Long_Long_Complex_Elementary_Functions;
