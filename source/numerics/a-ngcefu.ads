pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types;
generic
   with package Complex_Types is new Generic_Complex_Types (<>);
   use Complex_Types;
package Ada.Numerics.Generic_Complex_Elementary_Functions is
   pragma Pure;

   function Sqrt (X : Complex) return Complex;
   function Log (X : Complex) return Complex;
   function Exp (X : Complex) return Complex;
   function Exp (X : Imaginary) return Complex;
   function "**" (Left : Complex; Right : Complex) return Complex;
   function "**" (Left : Complex; Right : Real'Base) return Complex;
   function "**" (Left : Real'Base; Right : Complex) return Complex;

   pragma Inline (Sqrt);
   pragma Inline (Log);
   pragma Inline (Exp);
   pragma Inline ("**");

   function Sin (X : Complex) return Complex;
   function Cos (X : Complex) return Complex;
   function Tan (X : Complex) return Complex;
   function Cot (X : Complex) return Complex;

   pragma Inline (Sin);
   pragma Inline (Cos);
   pragma Inline (Tan);

   function Arcsin (X : Complex) return Complex;
   function Arccos (X : Complex) return Complex;
   function Arctan (X : Complex) return Complex;
   function Arccot (X : Complex) return Complex;

   pragma Inline (Arcsin);
   pragma Inline (Arccos);
   pragma Inline (Arctan);

   function Sinh (X : Complex) return Complex;
   function Cosh (X : Complex) return Complex;
   function Tanh (X : Complex) return Complex;
   function Coth (X : Complex) return Complex;

   pragma Inline (Sinh);
   pragma Inline (Cosh);
   pragma Inline (Tanh);

   function Arcsinh (X : Complex) return Complex;
   function Arccosh (X : Complex) return Complex;
   function Arctanh (X : Complex) return Complex;
   function Arccoth (X : Complex) return Complex;

   pragma Inline (Arcsinh);
   pragma Inline (Arccosh);
   pragma Inline (Arctanh);

end Ada.Numerics.Generic_Complex_Elementary_Functions;
