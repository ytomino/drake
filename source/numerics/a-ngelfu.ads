pragma License (Unrestricted);
generic
   type Float_Type is digits <>;
package Ada.Numerics.Generic_Elementary_Functions is
   pragma Pure;

   function Sqrt (X : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Sqrt);
   function Log (X : Float_Type'Base) return Float_Type'Base;
   function Log (X, Base : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Log);
   function Exp (X : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Exp);
   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base;
   pragma Inline ("**");

   function Sin (X : Float_Type'Base) return Float_Type'Base;
   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Sin);
   function Cos (X : Float_Type'Base) return Float_Type'Base;
   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Cos);
   function Tan (X : Float_Type'Base) return Float_Type'Base;
   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Tan);
   function Cot (X : Float_Type'Base) return Float_Type'Base;
   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base;

   function Arcsin (X : Float_Type'Base) return Float_Type'Base;
   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Arcsin);
   function Arccos (X : Float_Type'Base) return Float_Type'Base;
   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Arccos);
   function Arctan (Y : Float_Type'Base; X : Float_Type'Base := 1.0)
      return Float_Type'Base;
   function Arctan (
      Y : Float_Type'Base;
      X : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return Float_Type'Base;
   pragma Inline (Arctan);
   function Arccot (X : Float_Type'Base; Y : Float_Type'Base := 1.0)
      return Float_Type'Base;
   function Arccot (
      X : Float_Type'Base;
      Y : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return Float_Type'Base;
   pragma Inline (Arccot);

   function Sinh (X : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Sinh);
   function Cosh (X : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Cosh);
   function Tanh (X : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Tanh);
   function Coth (X : Float_Type'Base) return Float_Type'Base;
   function Arcsinh (X : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Arcsinh);
   function Arccosh (X : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Arccosh);
   function Arctanh (X : Float_Type'Base) return Float_Type'Base;
   pragma Inline (Arctanh);
   function Arccoth (X : Float_Type'Base) return Float_Type'Base;

end Ada.Numerics.Generic_Elementary_Functions;
