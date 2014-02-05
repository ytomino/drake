with Ada.Numerics.Generic_Arrays;
with System.Long_Long_Elementary_Functions;
package body Ada.Numerics.Generic_Real_Arrays is
   pragma Suppress (All_Checks);

   package Elementary_Functions is

      subtype Float_Type is Real;

      function Sqrt (X : Float_Type'Base) return Float_Type'Base;

   end Elementary_Functions;

   package body Elementary_Functions is

      function Sqrt (X : Float_Type'Base) return Float_Type'Base is
      begin
         if not Standard'Fast_Math and then X < 0.0 then
            raise Argument_Error; -- CXA5A10
         elsif Float_Type'Digits <= Float'Digits then
            declare
               function sqrtf (A1 : Float) return Float;
               pragma Import (Intrinsic, sqrtf, "__builtin_sqrtf");
            begin
               return Float_Type'Base (sqrtf (Float (X)));
            end;
         elsif Float_Type'Digits <= Long_Float'Digits then
            declare
               function sqrt (A1 : Long_Float) return Long_Float;
               pragma Import (Intrinsic, sqrt, "__builtin_sqrt");
            begin
               return Float_Type'Base (sqrt (Long_Float (X)));
            end;
         else
            return Float_Type'Base (
               System.Long_Long_Elementary_Functions.Fast_Sqrt (
                  Long_Long_Float (X)));
         end if;
      end Sqrt;

   end Elementary_Functions;

   function Minor is
      new Generic_Arrays.Minor (Real'Base, Real_Matrix);

   function Determinant_Body is
      new Generic_Arrays.Determinant (
         Real'Base,
         Real_Matrix,
         Zero => 0.0,
         One => 1.0);

   function Determinant (A : Real_Matrix) return Real'Base
      renames Determinant_Body;

   function Is_Minus (X : Real'Base) return Boolean;
   function Is_Minus (X : Real'Base) return Boolean is
   begin
      return X < 0.0;
   end Is_Minus;

   function Is_Small (X : Real'Base) return Boolean;
   function Is_Small (X : Real'Base) return Boolean is
   begin
      return abs X < 1.0e-32;
   end Is_Small;

   --  implementation

   procedure Eigensystem_Body is
      new Generic_Arrays.Eigensystem (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Matrix,
         Zero => 0.0,
         One => 1.0,
         Two => 2.0,
         Sqrt => Elementary_Functions.Sqrt,
         Is_Minus => Is_Minus,
         Is_Small => Is_Small,
         To_Real => "+");

   procedure Eigensystem (
      A : Real_Matrix;
      Values : out Real_Vector;
      Vectors : out Real_Matrix)
      renames Eigensystem_Body;

   function Eigenvalues (A : Real_Matrix) return Real_Vector is
      Vectors : Real_Matrix (A'Range (1), A'Range (2));
   begin
      return Result : Real_Vector (A'Range (2)) do
         Eigensystem (A, Result, Vectors);
      end return;
   end Eigenvalues;

   function Inverse_Body is
      new Generic_Arrays.Inverse (
         Real'Base,
         Real_Matrix,
         One => 1.0);

   function Inverse (A : Real_Matrix) return Real_Matrix
      renames Inverse_Body;

   function Solve (A : Real_Matrix; X : Real_Vector) return Real_Vector is
   begin
      return Inverse (A) * X;
   end Solve;

   function Solve (A, X : Real_Matrix) return Real_Matrix is
   begin
      return Inverse (A) * X;
   end Solve;

   function Transpose_Body is
      new Generic_Arrays.Transpose (Real'Base, Real_Matrix);

   function Transpose (X : Real_Matrix) return Real_Matrix
      renames Transpose_Body;

   function Unit_Matrix_Body is
      new Generic_Arrays.Unit_Matrix (
         Real'Base,
         Real_Matrix,
         Zero => 0.0,
         One => 1.0);

   function Unit_Matrix (
      Order : Positive;
      First_1, First_2 : Integer := 1)
      return Real_Matrix
      renames Unit_Matrix_Body;

   function Unit_Vector_Body is
      new Generic_Arrays.Unit_Vector (
         Real'Base,
         Real_Vector,
         Zero => 0.0,
         One => 1.0);

   function Unit_Vector (
      Index : Integer;
      Order : Positive;
      First : Integer := 1) return Real_Vector
      renames Unit_Vector_Body;

   function abs_Body is
      new Generic_Arrays.Operator_Vector (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         "abs");

   function "abs" (Right : Real_Vector) return Real_Vector
      renames abs_Body;

   function abs_Body is
      new Generic_Arrays.Absolute (
         Real'Base,
         Real_Vector,
         Real'Base,
         Zero => 0.0,
         Sqrt => Elementary_Functions.Sqrt);

   function "abs" (Right : Real_Vector) return Real'Base
      renames abs_Body;

   function abs_Body is
      new Generic_Arrays.Operator_Matrix (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         "abs");

   function "abs" (Right : Real_Matrix) return Real_Matrix
      renames abs_Body;

   function "+" (Right : Real_Vector) return Real_Vector is
   begin
      return Right;
   end "+";

   function add_Body is
      new Generic_Arrays.Operator_Vector_Vector (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         "+");

   function "+" (Left, Right : Real_Vector) return Real_Vector
      renames add_Body;

   function "+" (Right : Real_Matrix) return Real_Matrix is
   begin
      return Right;
   end "+";

   function add_Body is
      new Generic_Arrays.Operator_Matrix_Matrix (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         "+");

   function "+" (Left, Right : Real_Matrix) return Real_Matrix
      renames add_Body;

   function neg_Body is
      new Generic_Arrays.Operator_Vector (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         "-");

   function "-" (Right : Real_Vector) return Real_Vector
      renames neg_Body;

   function sub_Body is
      new Generic_Arrays.Operator_Vector_Vector (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         "-");

   function "-" (Left, Right : Real_Vector) return Real_Vector
      renames sub_Body;

   function neg_Body is
      new Generic_Arrays.Operator_Matrix (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         "-");

   function "-" (Right : Real_Matrix) return Real_Matrix
      renames neg_Body;

   function sub_Body is
      new Generic_Arrays.Operator_Matrix_Matrix (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         "-");

   function "-" (Left, Right : Real_Matrix) return Real_Matrix
      renames sub_Body;

   function mul_Body is
      new Generic_Arrays.Inner_Production (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         Real'Base,
         Zero => 0.0);

   function "*" (Left, Right : Real_Vector) return Real'Base
      renames mul_Body;

   function "*" (Left : Real'Base; Right : Real_Vector) return Real_Vector is
   begin
      return Right * Left;
   end "*";

   function mul_Body is
      new Generic_Arrays.Operator_Vector_Param (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real'Base,
         Real_Vector,
         "*");

   function "*" (Left : Real_Vector; Right : Real'Base) return Real_Vector
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Matrix_Matrix (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         Zero => 0.0);

   function "*" (Left, Right : Real_Matrix) return Real_Matrix
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Vector_Vector (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Matrix);

   function "*" (Left, Right : Real_Vector) return Real_Matrix
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Vector_Matrix (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Vector,
         Zero => 0.0);

   function "*" (Left : Real_Vector; Right : Real_Matrix) return Real_Vector
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Matrix_Vector (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         Zero => 0.0);

   function "*" (Left : Real_Matrix; Right : Real_Vector) return Real_Vector
      renames mul_Body;

   function "*" (Left : Real'Base; Right : Real_Matrix) return Real_Matrix is
   begin
      return Right * Left;
   end "*";

   function mul_Body is
      new Generic_Arrays.Operator_Matrix_Param (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real'Base,
         Real_Matrix,
         "*");

   function "*" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix
      renames mul_Body;

   function "/" (Left : Real_Vector; Right : Real'Base) return Real_Vector is
   begin
      return Left * (1.0 / Right);
   end "/";

   function "/" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix is
   begin
      return Left * (1.0 / Right);
   end "/";

end Ada.Numerics.Generic_Real_Arrays;
