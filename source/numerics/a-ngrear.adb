with Ada.Numerics.Generic_Arrays;
package body Ada.Numerics.Generic_Real_Arrays is

   package Elementary_Functions is

      subtype Float_Type is Real;

      function Sqrt (X : Float_Type'Base) return Float_Type'Base;

   end Elementary_Functions;

   package body Elementary_Functions is

      function Sqrt (X : Float_Type'Base) return Float_Type'Base is
      begin
         if not Standard'Fast_Math and then not (X >= 0.0) then
            raise Argument_Error; -- CXA5A10
         end if;
         if Float_Type'Digits <= Float'Digits then
            declare
               function sqrtf (A1 : Float) return Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_sqrtf";
            begin
               return Float_Type'Base (sqrtf (Float (X)));
            end;
         elsif Float_Type'Digits <= Long_Float'Digits then
            declare
               function sqrt (A1 : Long_Float) return Long_Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_sqrt";
            begin
               return Float_Type'Base (sqrt (Long_Float (X)));
            end;
         else
            declare
               function sqrtl (x : Long_Long_Float) return Long_Long_Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_sqrtl";
            begin
               return Float_Type'Base (sqrtl (Long_Long_Float (X)));
            end;
         end if;
      end Sqrt;

   end Elementary_Functions;

   function Minor is new Generic_Arrays.Minor (Real'Base, Real_Matrix);
      --  for Inverse and Determinant

   --  implementation

   function "+" (Right : Real_Vector) return Real_Vector is
   begin
      return Right;
   end "+";

   function "-" (Right : Real_Vector) return Real_Vector is
      function neg_Body is
         new Generic_Arrays.Operator_Vector (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            "-");
   begin
      return neg_Body (Right);
   end "-";

   function "abs" (Right : Real_Vector) return Real_Vector is
      function abs_Body is
         new Generic_Arrays.Operator_Vector (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            "abs");
   begin
      return abs_Body (Right);
   end "abs";

   function "+" (Left, Right : Real_Vector) return Real_Vector is
      function add_Body is
         new Generic_Arrays.Operator_Vector_Vector (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            "+");
   begin
      return add_Body (Left, Right);
   end "+";

   function "-" (Left, Right : Real_Vector) return Real_Vector is
      function sub_Body is
         new Generic_Arrays.Operator_Vector_Vector (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            "-");
   begin
      return sub_Body (Left, Right);
   end "-";

   function "*" (Left, Right : Real_Vector) return Real'Base is
      function mul_Body is
         new Generic_Arrays.Inner_Production (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            Real'Base,
            Zero => 0.0);
   begin
      return mul_Body (Left, Right);
   end "*";

   function "abs" (Right : Real_Vector) return Real'Base is
      function abs_Body is
         new Generic_Arrays.Absolute (
            Real'Base,
            Real_Vector,
            Real'Base,
            Zero => 0.0,
            Sqrt => Elementary_Functions.Sqrt);
   begin
      return abs_Body (Right);
   end "abs";

   function "*" (Left : Real'Base; Right : Real_Vector) return Real_Vector is
   begin
      return Right * Left;
   end "*";

   function "*" (Left : Real_Vector; Right : Real'Base) return Real_Vector is
      function mul_Body is
         new Generic_Arrays.Operator_Vector_Param (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real'Base,
            Real_Vector,
            "*");
   begin
      return mul_Body (Left, Right);
   end "*";

   function "/" (Left : Real_Vector; Right : Real'Base) return Real_Vector is
   begin
      return Left * (1.0 / Right);
   end "/";

   function Unit_Vector (
      Index : Integer;
      Order : Positive;
      First : Integer := 1)
      return Real_Vector
   is
      function Unit_Vector_Body is
         new Generic_Arrays.Unit_Vector (
            Real'Base,
            Real_Vector,
            Zero => 0.0,
            One => 1.0);
   begin
      return Unit_Vector_Body (Index, Order, First);
   end Unit_Vector;

   function "+" (Right : Real_Matrix) return Real_Matrix is
   begin
      return Right;
   end "+";

   function "-" (Right : Real_Matrix) return Real_Matrix is
      function neg_Body is
         new Generic_Arrays.Operator_Matrix (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            "-");
   begin
      return neg_Body (Right);
   end "-";

   function "abs" (Right : Real_Matrix) return Real_Matrix is
      function abs_Body is
         new Generic_Arrays.Operator_Matrix (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            "abs");
   begin
      return abs_Body (Right);
   end "abs";

   function Transpose (X : Real_Matrix) return Real_Matrix is
      function Transpose_Body is
         new Generic_Arrays.Transpose (Real'Base, Real_Matrix);
   begin
      return Transpose_Body (X);
   end Transpose;

   function "+" (Left, Right : Real_Matrix) return Real_Matrix is
      function add_Body is
         new Generic_Arrays.Operator_Matrix_Matrix (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            "+");
   begin
      return add_Body (Left, Right);
   end "+";

   function "-" (Left, Right : Real_Matrix) return Real_Matrix is
      function sub_Body is
         new Generic_Arrays.Operator_Matrix_Matrix (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            "-");
   begin
      return sub_Body (Left, Right);
   end "-";

   function "*" (Left, Right : Real_Matrix) return Real_Matrix is
      function mul_Body is
         new Generic_Arrays.Multiply_Matrix_Matrix (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            Zero => 0.0);
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left, Right : Real_Vector) return Real_Matrix is
      function mul_Body is
         new Generic_Arrays.Multiply_Vector_Vector (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Matrix);
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Real_Vector; Right : Real_Matrix) return Real_Vector is
      function mul_Body is
         new Generic_Arrays.Multiply_Vector_Matrix (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Vector,
            Zero => 0.0);
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Real_Matrix; Right : Real_Vector) return Real_Vector is
      function mul_Body is
         new Generic_Arrays.Multiply_Matrix_Vector (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            Zero => 0.0);
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Real'Base; Right : Real_Matrix) return Real_Matrix is
   begin
      return Right * Left;
   end "*";

   function "*" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix is
      function mul_Body is
         new Generic_Arrays.Operator_Matrix_Param (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real'Base,
            Real_Matrix,
            "*");
   begin
      return mul_Body (Left, Right);
   end "*";

   function "/" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix is
   begin
      return Left * (1.0 / Right);
   end "/";

   function Solve (A : Real_Matrix; X : Real_Vector) return Real_Vector is
   begin
      return Inverse (A) * X;
   end Solve;

   function Solve (A, X : Real_Matrix) return Real_Matrix is
   begin
      return Inverse (A) * X;
   end Solve;

   function Inverse (A : Real_Matrix) return Real_Matrix is
      function Inverse_Body is
         new Generic_Arrays.Inverse (
            Real'Base,
            Real_Matrix,
            One => 1.0);
   begin
      return Inverse_Body (A);
   end Inverse;

   function Determinant (A : Real_Matrix) return Real'Base is
      function Determinant_Body is
         new Generic_Arrays.Determinant (
            Real'Base,
            Real_Matrix,
            Zero => 0.0,
            One => 1.0)
         with Convention => Intrinsic;
   begin
      return Determinant_Body (A);
   end Determinant;

   function Eigenvalues (A : Real_Matrix) return Real_Vector is
      Vectors : Real_Matrix (A'Range (1), A'Range (2));
   begin
      return Result : Real_Vector (A'Range (2)) do
         Eigensystem (A, Result, Vectors);
      end return;
   end Eigenvalues;

   procedure Eigensystem (
      A : Real_Matrix;
      Values : out Real_Vector;
      Vectors : out Real_Matrix)
   is
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
   begin
      Eigensystem_Body (A, Values, Vectors);
   end Eigensystem;

   function Unit_Matrix (Order : Positive; First_1, First_2 : Integer := 1)
      return Real_Matrix
   is
      function Unit_Matrix_Body is
         new Generic_Arrays.Unit_Matrix (
            Real'Base,
            Real_Matrix,
            Zero => 0.0,
            One => 1.0);
   begin
      return Unit_Matrix_Body (Order, First_1, First_2);
   end Unit_Matrix;

end Ada.Numerics.Generic_Real_Arrays;
