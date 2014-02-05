with Ada.Unchecked_Conversion;
with System.Long_Long_Complex_Types;
with System.Long_Long_Complex_Elementary_Functions;
package body Ada.Numerics.Generic_Complex_Elementary_Functions is
   pragma Suppress (All_Checks);

   pragma Warnings (Off);
   function To_Complex is
      new Unchecked_Conversion (
         Complex,
         System.Long_Long_Complex_Types.Complex);
   function To_Long_Complex is
      new Unchecked_Conversion (
         Complex,
         System.Long_Long_Complex_Types.Long_Complex);
   function To_Long_Long_Complex is
      new Unchecked_Conversion (
         Complex,
         System.Long_Long_Complex_Types.Long_Long_Complex);
   function From_Complex is
      new Unchecked_Conversion (
         System.Long_Long_Complex_Types.Complex,
         Complex);
   function From_Long_Complex is
      new Unchecked_Conversion (
         System.Long_Long_Complex_Types.Long_Complex,
         Complex);
   function From_Long_Long_Complex is
      new Unchecked_Conversion (
         System.Long_Long_Complex_Types.Long_Long_Complex,
         Complex);
   pragma Warnings (On);

   --  implementation

   function Sqrt (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Sqrt (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Sqrt (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Sqrt (
               To_Long_Long_Complex (X)));
      end if;
   end Sqrt;

   function Log (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Log (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Log (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Log (
               To_Long_Long_Complex (X)));
      end if;
   end Log;

   function Exp (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Exp (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Exp (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Exp (
               To_Long_Long_Complex (X)));
      end if;
   end Exp;

   function Exp (X : Imaginary) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Exp (
               System.Long_Long_Complex_Types.Imaginary (Im (X))));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Exp (
               System.Long_Long_Complex_Types.Long_Imaginary (Im (X))));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Exp (
               System.Long_Long_Complex_Types.Long_Long_Imaginary (Im (X))));
      end if;
   end Exp;

   function "**" (Left : Complex; Right : Complex) return Complex is
   begin
      if not Standard'Fast_Math
         and then Left.Re = 0.0 and then Left.Im = 0.0
         and then Right.Re = 0.0 and then Right.Im = 0.0
      then
         raise Argument_Error; -- CXG1004
      elsif not Standard'Fast_Math
         and then Right.Re = 1.0 and then Right.Im = 0.0
      then
         return Left; -- CXG1005
      elsif Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Pow (
               To_Complex (Left),
               To_Complex (Right)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Pow (
               To_Long_Complex (Left),
               To_Long_Complex (Right)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Pow (
               To_Long_Long_Complex (Left),
               To_Long_Long_Complex (Right)));
      end if;
   end "**";

   function "**" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return Left ** (Re => Right, Im => 0.0);
   end "**";

   function "**" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left, Im => 0.0) ** Right;
   end "**";

   function Sin (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Sin (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Sin (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Sin (
               To_Long_Long_Complex (X)));
      end if;
   end Sin;

   function Cos (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Cos (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Cos (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Cos (
               To_Long_Long_Complex (X)));
      end if;
   end Cos;

   function Tan (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Tan (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Tan (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Tan (
               To_Long_Long_Complex (X)));
      end if;
   end Tan;

   function Cot (X : Complex) return Complex is
   begin
      return Cos (X) / Sin (X);
   end Cot;

   function Arcsin (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arcsin (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arcsin (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arcsin (
               To_Long_Long_Complex (X)));
      end if;
   end Arcsin;

   function Arccos (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arccos (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arccos (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arccos (
               To_Long_Long_Complex (X)));
      end if;
   end Arccos;

   function Arctan (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arctan (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arctan (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arctan (
               To_Long_Long_Complex (X)));
      end if;
   end Arctan;

   function Arccot (X : Complex) return Complex is
   begin
      return Pi / 2.0 - Arctan (X);
   end Arccot;

   function Sinh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Sinh (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Sinh (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Sinh (
               To_Long_Long_Complex (X)));
      end if;
   end Sinh;

   function Cosh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Cosh (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Cosh (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Cosh (
               To_Long_Long_Complex (X)));
      end if;
   end Cosh;

   function Tanh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Tanh (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Tanh (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Tanh (
               To_Long_Long_Complex (X)));
      end if;
   end Tanh;

   function Coth (X : Complex) return Complex is
   begin
      return Cosh (X) / Sinh (X);
   end Coth;

   function Arcsinh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arcsinh (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arcsinh (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arcsinh (
               To_Long_Long_Complex (X)));
      end if;
   end Arcsinh;

   function Arccosh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arccosh (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arccosh (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arccosh (
               To_Long_Long_Complex (X)));
      end if;
   end Arccosh;

   function Arctanh (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         return From_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arctanh (
               To_Complex (X)));
      elsif Real'Digits <= Long_Float'Digits then
         return From_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arctanh (
               To_Long_Complex (X)));
      else
         return From_Long_Long_Complex (
            System.Long_Long_Complex_Elementary_Functions.Fast_Arctanh (
               To_Long_Long_Complex (X)));
      end if;
   end Arctanh;

   function Arccoth (X : Complex) return Complex is
   begin
      if X.Re = 0.0 and then X.Im = 0.0 then
         return (0.0, Pi / 2.0);
      else
         return Arctanh (1.0 / X);
      end if;
   end Arccoth;

end Ada.Numerics.Generic_Complex_Elementary_Functions;
