pragma License (Unrestricted);
generic
   type Real is digits <>;
package Ada.Numerics.Generic_Complex_Types is
   pragma Pure;

   type Complex is record
      Re, Im : Real'Base;
   end record;
   pragma Complex_Representation (Complex);

   type Imaginary is private;
   pragma Preelaborable_Initialization (Imaginary);

   --  modified
--  i : constant Imaginary;
--  j : constant Imaginary;
   function i return Imaginary;
   function j return Imaginary
      renames i;

   pragma Inline (i);

   function Re (X : Complex) return Real'Base;
   function Im (X : Complex) return Real'Base;
   function Im (X : Imaginary) return Real'Base;

   pragma Inline (Re);
   pragma Inline (Im);

   procedure Set_Re (X : in out Complex; Re : Real'Base);
   procedure Set_Im (X : in out Complex; Im : Real'Base);
   procedure Set_Im (X : out Imaginary; Im : Real'Base);

   pragma Inline (Set_Re);
   pragma Inline (Set_Im);

   function Compose_From_Cartesian (Re, Im : Real'Base) return Complex;
   function Compose_From_Cartesian (Re : Real'Base) return Complex;
   function Compose_From_Cartesian (Im : Imaginary) return Complex;

   pragma Inline (Compose_From_Cartesian);

   function Modulus (X : Complex) return Real'Base;
   function "abs" (Right : Complex) return Real'Base
      renames Modulus;

   pragma Inline (Modulus);

   function Argument (X : Complex) return Real'Base;
   function Argument (X : Complex; Cycle : Real'Base) return Real'Base;

   pragma Inline (Argument);

   function Compose_From_Polar (Modulus, Argument : Real'Base)
      return Complex;
   function Compose_From_Polar (Modulus, Argument, Cycle : Real'Base)
      return Complex;

   function "+" (Right : Complex) return Complex;
   function "-" (Right : Complex) return Complex;
   function Conjugate (X : Complex) return Complex;

   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline (Conjugate);

   function "+" (Left, Right : Complex) return Complex;
   function "-" (Left, Right : Complex) return Complex;
   function "*" (Left, Right : Complex) return Complex;
   function "/" (Left, Right : Complex) return Complex;

   pragma Inline ("+");
   pragma Inline ("-");

   function "**" (Left : Complex; Right : Integer) return Complex;

   function "+" (Right : Imaginary) return Imaginary
      with Import, Convention => Intrinsic;
   function "-" (Right : Imaginary) return Imaginary
      with Import, Convention => Intrinsic;
   function Conjugate (X : Imaginary) return Imaginary
      renames "-";
   function "abs" (Right : Imaginary) return Real'Base;

   pragma Inline ("abs");

   function "+" (Left, Right : Imaginary) return Imaginary
      with Import, Convention => Intrinsic;
   function "-" (Left, Right : Imaginary) return Imaginary
      with Import, Convention => Intrinsic;
   function "*" (Left, Right : Imaginary) return Real'Base;
   function "/" (Left, Right : Imaginary) return Real'Base;

   pragma Inline ("*");
   pragma Inline ("/");

   function "**" (Left : Imaginary; Right : Integer) return Complex;

   function "<" (Left, Right : Imaginary) return Boolean
      with Import, Convention => Intrinsic;
   function "<=" (Left, Right : Imaginary) return Boolean
      with Import, Convention => Intrinsic;
   function ">" (Left, Right : Imaginary) return Boolean
      with Import, Convention => Intrinsic;
   function ">=" (Left, Right : Imaginary) return Boolean
      with Import, Convention => Intrinsic;

   function "+" (Left : Complex; Right : Real'Base) return Complex;
   function "+" (Left : Real'Base; Right : Complex) return Complex;
   function "-" (Left : Complex; Right : Real'Base) return Complex;
   function "-" (Left : Real'Base; Right : Complex) return Complex;
   function "*" (Left : Complex; Right : Real'Base) return Complex;
   function "*" (Left : Real'Base; Right : Complex) return Complex;
   function "/" (Left : Complex; Right : Real'Base) return Complex;
   function "/" (Left : Real'Base; Right : Complex) return Complex;

   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");
   pragma Inline ("/");

   function "+" (Left : Complex; Right : Imaginary) return Complex;
   function "+" (Left : Imaginary; Right : Complex) return Complex;
   function "-" (Left : Complex; Right : Imaginary) return Complex;
   function "-" (Left : Imaginary; Right : Complex) return Complex;
   function "*" (Left : Complex; Right : Imaginary) return Complex;
   function "*" (Left : Imaginary; Right : Complex) return Complex;
   function "/" (Left : Complex; Right : Imaginary) return Complex;
   function "/" (Left : Imaginary; Right : Complex) return Complex;

   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");
   pragma Inline ("/");

   function "+" (Left : Imaginary; Right : Real'Base) return Complex;
   function "+" (Left : Real'Base; Right : Imaginary) return Complex;
   function "-" (Left : Imaginary; Right : Real'Base) return Complex;
   function "-" (Left : Real'Base; Right : Imaginary) return Complex;
   function "*" (Left : Imaginary; Right : Real'Base) return Imaginary;
   function "*" (Left : Real'Base; Right : Imaginary) return Imaginary;
   function "/" (Left : Imaginary; Right : Real'Base) return Imaginary;
   function "/" (Left : Real'Base; Right : Imaginary) return Imaginary;

   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");
   pragma Inline ("/");

private

   type Imaginary is new Real'Base;
--  i : constant Imaginary := 1.0;
--  j : constant Imaginary := 1.0;
   function i return Imaginary is (1.0);

end Ada.Numerics.Generic_Complex_Types;
