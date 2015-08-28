pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD
with System.Long_Long_Complex_Types;
package System.Long_Long_Complex_Elementary_Functions is
   pragma Pure;

   --  Complex

   subtype Imaginary is Long_Long_Complex_Types.Imaginary;
   subtype Complex is Long_Long_Complex_Types.Complex;

   function Fast_Log (X : Complex) return Complex;
   pragma Inline (Fast_Log);

   function Fast_Exp (X : Complex) return Complex;
   function Fast_Exp (X : Imaginary) return Complex;
   pragma Inline (Fast_Exp);

   function Fast_Pow (Left, Right : Complex) return Complex;
   pragma Inline (Fast_Pow);

   function Fast_Sin (X : Complex) return Complex;
   pragma Inline (Fast_Sin);

   function Fast_Cos (X : Complex) return Complex;
   pragma Inline (Fast_Cos);

   function Fast_Tan (X : Complex) return Complex;
   pragma Inline (Fast_Tan);

   function Fast_Arcsin (X : Complex) return Complex;
   pragma Inline (Fast_Arcsin);

   function Fast_Arccos (X : Complex) return Complex;
   pragma Inline (Fast_Arccos);

   function Fast_Arctan (X : Complex) return Complex;
   pragma Inline (Fast_Arctan);

   function Fast_Sinh (X : Complex) return Complex;
   pragma Inline (Fast_Sinh);

   function Fast_Cosh (X : Complex) return Complex;
   pragma Inline (Fast_Cosh);

   function Fast_Tanh (X : Complex) return Complex;
   pragma Inline (Fast_Tanh);

   function Fast_Arcsinh (X : Complex) return Complex;
   pragma Inline (Fast_Arcsinh);

   function Fast_Arccosh (X : Complex) return Complex;
   pragma Inline (Fast_Arccosh);

   function Fast_Arctanh (X : Complex) return Complex;
   pragma Inline (Fast_Arctanh);

   --  Long_Complex

   subtype Long_Imaginary is Long_Long_Complex_Types.Long_Imaginary;
   subtype Long_Complex is Long_Long_Complex_Types.Long_Complex;

   function Fast_Log (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Log);

   function Fast_Exp (X : Long_Complex) return Long_Complex;
   function Fast_Exp (X : Long_Imaginary) return Long_Complex;
   pragma Inline (Fast_Exp);

   function Fast_Pow (Left, Right : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Pow);

   function Fast_Sin (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Sin);

   function Fast_Cos (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Cos);

   function Fast_Tan (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Tan);

   function Fast_Arcsin (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Arcsin);

   function Fast_Arccos (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Arccos);

   function Fast_Arctan (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Arctan);

   function Fast_Sinh (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Sinh);

   function Fast_Cosh (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Cosh);

   function Fast_Tanh (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Tanh);

   function Fast_Arcsinh (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Arcsinh);

   function Fast_Arccosh (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Arccosh);

   function Fast_Arctanh (X : Long_Complex) return Long_Complex;
   pragma Inline (Fast_Arctanh);

   --  Long_Long_Complex

   subtype Long_Long_Imaginary is Long_Long_Complex_Types.Long_Long_Imaginary;
   subtype Long_Long_Complex is Long_Long_Complex_Types.Long_Long_Complex;

   function csqrtl (x : Long_Long_Complex) return Long_Long_Complex
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_csqrtl";
   function Fast_Sqrt (X : Long_Long_Complex) return Long_Long_Complex
      renames csqrtl;

   function Fast_Log (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Exp (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Exp (X : Long_Long_Imaginary) return Long_Long_Complex;
   function Fast_Pow (Left, Right : Long_Long_Complex)
      return Long_Long_Complex;

   function Fast_Sin (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Cos (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Tan (X : Long_Long_Complex) return Long_Long_Complex;

   function Fast_Arcsin (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Arccos (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Arctan (X : Long_Long_Complex) return Long_Long_Complex;

   function Fast_Sinh (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Cosh (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Tanh (X : Long_Long_Complex) return Long_Long_Complex;

   function Fast_Arcsinh (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Arccosh (X : Long_Long_Complex) return Long_Long_Complex;
   function Fast_Arctanh (X : Long_Long_Complex) return Long_Long_Complex;

end System.Long_Long_Complex_Elementary_Functions;
