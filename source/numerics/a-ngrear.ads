pragma License (Unrestricted);
generic
   type Real is digits <>;
package Ada.Numerics.Generic_Real_Arrays is
   pragma Pure;

   --  Types

   type Real_Vector is array (Integer range <>) of Real'Base;
   for Real_Vector'Alignment use Standard'Maximum_Alignment;
   type Real_Matrix is array (Integer range <>, Integer range <>) of Real'Base;
   for Real_Matrix'Alignment use Standard'Maximum_Alignment;

   --  Subprograms for Real_Vector types

   --  Real_Vector arithmetic operations

   function "+" (Right : Real_Vector) return Real_Vector;
   function "-" (Right : Real_Vector) return Real_Vector;
   function "abs" (Right : Real_Vector) return Real_Vector;

   function "+" (Left, Right : Real_Vector) return Real_Vector;
   function "-" (Left, Right : Real_Vector) return Real_Vector;

   function "*" (Left, Right : Real_Vector) return Real'Base;

   function "abs" (Right : Real_Vector) return Real'Base;

   --  Real_Vector scaling operations

   function "*" (Left : Real'Base; Right : Real_Vector) return Real_Vector;
   function "*" (Left : Real_Vector; Right : Real'Base) return Real_Vector;
   function "/" (Left : Real_Vector; Right : Real'Base) return Real_Vector;

   --  Other Real_Vector operations

   function Unit_Vector (
      Index : Integer;
      Order : Positive;
      First : Integer := 1)
      return Real_Vector;

   --  Subprograms for Real_Matrix types

   --  Real_Matrix arithmetic operations

   function "+" (Right : Real_Matrix) return Real_Matrix;
   function "-" (Right : Real_Matrix) return Real_Matrix;
   function "abs" (Right : Real_Matrix) return Real_Matrix;
   function Transpose (X : Real_Matrix) return Real_Matrix;

   function "+" (Left, Right : Real_Matrix) return Real_Matrix;
   function "-" (Left, Right : Real_Matrix) return Real_Matrix;
   function "*" (Left, Right : Real_Matrix) return Real_Matrix;

   function "*" (Left, Right : Real_Vector) return Real_Matrix;

   function "*" (Left : Real_Vector; Right : Real_Matrix) return Real_Vector;
   function "*" (Left : Real_Matrix; Right : Real_Vector) return Real_Vector;

   --  Real_Matrix scaling operations

   function "*" (Left : Real'Base; Right : Real_Matrix) return Real_Matrix;
   function "*" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix;
   function "/" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix;

   --  Real_Matrix inversion and related operations

   function Solve (A : Real_Matrix; X : Real_Vector) return Real_Vector;
   function Solve (A, X : Real_Matrix) return Real_Matrix;
   function Inverse (A : Real_Matrix) return Real_Matrix;
   function Determinant (A : Real_Matrix) return Real'Base;

   --  Eigenvalues and vectors of a real symmetric matrix

   function Eigenvalues (A : Real_Matrix) return Real_Vector;
   procedure Eigensystem (
      A : Real_Matrix;
      Values : out Real_Vector;
      Vectors : out Real_Matrix);

   --  Other Real_Matrix operations

   function Unit_Matrix (Order : Positive; First_1, First_2 : Integer := 1)
      return Real_Matrix;

end Ada.Numerics.Generic_Real_Arrays;
