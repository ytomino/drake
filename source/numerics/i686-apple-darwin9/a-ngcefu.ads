pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types;
generic
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (<>);
   use Complex_Types;
package Ada.Numerics.Generic_Complex_Elementary_Functions is
   pragma Pure;

   function Sqrt (X : Complex) return Complex;
   pragma Inline (Sqrt);
   function Log (X : Complex) return Complex;
   pragma Inline (Log);
   function Exp (X : Complex) return Complex;
   function Exp (X : Imaginary) return Complex;
   pragma Inline (Exp);
   function "**" (Left : Complex; Right : Complex) return Complex;
   function "**" (Left : Complex; Right : Real'Base) return Complex;
   function "**" (Left : Real'Base; Right : Complex) return Complex;
   pragma Inline ("**");

   function Sin (X : Complex) return Complex;
   pragma Inline (Sin);
   function Cos (X : Complex) return Complex;
   pragma Inline (Cos);
   function Tan (X : Complex) return Complex;
   pragma Inline (Tan);
   function Cot (X : Complex) return Complex;

   function Arcsin (X : Complex) return Complex;
   pragma Inline (Arcsin);
   function Arccos (X : Complex) return Complex;
   pragma Inline (Arccos);
   function Arctan (X : Complex) return Complex;
   pragma Inline (Arctan);
   function Arccot (X : Complex) return Complex;

   function Sinh (X : Complex) return Complex;
   pragma Inline (Sinh);
   function Cosh (X : Complex) return Complex;
   pragma Inline (Cosh);
   function Tanh (X : Complex) return Complex;
   pragma Inline (Tanh);
   function Coth (X : Complex) return Complex;

   function Arcsinh (X : Complex) return Complex;
   pragma Inline (Arcsinh);
   function Arccosh (X : Complex) return Complex;
   pragma Inline (Arccosh);
   function Arctanh (X : Complex) return Complex;
   pragma Inline (Arctanh);
   function Arccoth (X : Complex) return Complex;

end Ada.Numerics.Generic_Complex_Elementary_Functions;
