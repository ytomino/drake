with Ada.Numerics.Generic_Arrays;
package body Ada.Numerics.Generic_Complex_Arrays is
   pragma Suppress (All_Checks);

   package Elementary_Functions is

      subtype Float_Type is Real_Arrays.Real;

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

   package Complex_Elementary_Functions is

      subtype Real is Real_Arrays.Real;

      function Sqrt (X : Complex) return Complex;

   end Complex_Elementary_Functions;

   package body Complex_Elementary_Functions is

      function Sqrt (X : Complex) return Complex is
      begin
         if Real'Digits <= Float'Digits then
            declare
               function csqrtf (x : Complex) return Complex
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_csqrtf";
            begin
               return csqrtf (X);
            end;
         elsif Real'Digits <= Long_Float'Digits
            and then Real'Size <= Long_Float'Size -- for 32bit FreeBSD
         then
            declare
               function csqrt (x : Complex) return Complex
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_csqrt";
            begin
               return csqrt (X);
            end;
         else
            declare
               function csqrtl (x : Complex) return Complex
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_csqrtl";
            begin
               return csqrtl (X);
            end;
         end if;
      end Sqrt;

   end Complex_Elementary_Functions;

   function Minor is
      new Generic_Arrays.Minor (Complex, Complex_Matrix);

   --  implementation

   function Argument_Body is
      new Generic_Arrays.Operator_Vector (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Argument);

   function Argument (X : Complex_Vector) return Real_Vector
      renames Argument_Body;

   function Argument_Body is
      new Generic_Arrays.Operator_Vector_Param (
         Complex,
         Complex_Vector,
         Real'Base,
         Real'Base,
         Real_Vector,
         Argument);

   function Argument (X : Complex_Vector; Cycle : Real'Base) return Real_Vector
      renames Argument_Body;

   function Argument_Body is
      new Generic_Arrays.Operator_Matrix (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Matrix,
         Argument);

   function Argument (X : Complex_Matrix) return Real_Matrix
      renames Argument_Body;

   function Argument_Body is
      new Generic_Arrays.Operator_Matrix_Param (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real'Base,
         Real_Matrix,
         Argument);

   function Argument (X : Complex_Matrix; Cycle : Real'Base) return Real_Matrix
      renames Argument_Body;

   function Compose_From_Cartesian_Body is
      new Generic_Arrays.Operator_Vector (
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Vector,
         Compose_From_Cartesian);

   function Compose_From_Cartesian (Re : Real_Vector) return Complex_Vector
      renames Compose_From_Cartesian_Body;

   function Compose_From_Cartesian_Body is
      new Generic_Arrays.Operator_Vector_Vector (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Vector,
         Compose_From_Cartesian);

   function Compose_From_Cartesian (Re, Im : Real_Vector) return Complex_Vector
      renames Compose_From_Cartesian_Body;

   function Compose_From_Cartesian_Body is
      new Generic_Arrays.Operator_Matrix (
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Matrix,
         Compose_From_Cartesian);

   function Compose_From_Cartesian (Re : Real_Matrix) return Complex_Matrix
      renames Compose_From_Cartesian_Body;

   function Compose_From_Cartesian_Body is
      new Generic_Arrays.Operator_Matrix_Matrix (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Matrix,
         Compose_From_Cartesian);

   function Compose_From_Cartesian (Re, Im : Real_Matrix) return Complex_Matrix
      renames Compose_From_Cartesian_Body;

   function Compose_From_Polar_Body is
      new Generic_Arrays.Operator_Vector_Vector (
         Real'Base,
         Real_Vector,
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Vector,
         Compose_From_Polar);

   function Compose_From_Polar (Modulus, Argument : Real_Vector)
      return Complex_Vector
      renames Compose_From_Polar_Body;

   function Compose_From_Polar_Body is
      new Generic_Arrays.Operator_Vector_Vector_Param (
         Real'Base,
         Real_Vector,
         Real'Base,
         Complex,
         Complex_Vector,
         Compose_From_Polar);

   function Compose_From_Polar (
      Modulus, Argument : Real_Vector;
      Cycle : Real'Base)
      return Complex_Vector
      renames Compose_From_Polar_Body;

   function Compose_From_Polar_Body is
      new Generic_Arrays.Operator_Matrix_Matrix (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Matrix,
         Compose_From_Polar);

   function Compose_From_Polar (Modulus, Argument : Real_Matrix)
      return Complex_Matrix
      renames Compose_From_Polar_Body;

   function Compose_From_Polar_Body is
      new Generic_Arrays.Operator_Matrix_Matrix_Param (
         Real'Base,
         Real_Matrix,
         Real'Base,
         Complex,
         Complex_Matrix,
         Compose_From_Polar);

   function Compose_From_Polar (
      Modulus, Argument : Real_Matrix;
      Cycle : Real'Base)
      return Complex_Matrix
      renames Compose_From_Polar_Body;

   function Conjugate_Body is
      new Generic_Arrays.Operator_Vector (
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         Conjugate);

   function Conjugate (X : Complex_Vector) return Complex_Vector
      renames Conjugate_Body;

   function Conjugate_Body is
      new Generic_Arrays.Operator_Matrix (
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         Conjugate);

   function Conjugate (X : Complex_Matrix) return Complex_Matrix
      renames Conjugate_Body;

   function Determinant_Body is
      new Generic_Arrays.Determinant (
         Complex,
         Complex_Matrix,
         Zero => (Re => 0.0, Im => 0.0),
         One => (Re => 1.0, Im => 0.0));

   function Determinant (A : Complex_Matrix) return Complex
      renames Determinant_Body;

   function Is_Minus (X : Complex) return Boolean;
   function Is_Minus (X : Complex) return Boolean is
   begin
      return X.Re < 0.0;
   end Is_Minus;

   function Is_Small (X : Complex) return Boolean;
   function Is_Small (X : Complex) return Boolean is
   begin
      return abs X < 1.0e-32;
   end Is_Small;

   procedure Eigensystem_Body is
      new Generic_Arrays.Eigensystem (
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Matrix,
         Zero => (Re => 0.0, Im => 0.0),
         One => (Re => 1.0, Im => 0.0),
         Two => (Re => 2.0, Im => 0.0),
         Sqrt => Complex_Elementary_Functions.Sqrt,
         Is_Minus => Is_Minus,
         Is_Small => Is_Small,
         To_Real => Re);

   procedure Eigensystem (
      A : Complex_Matrix;
      Values : out Real_Vector;
      Vectors : out Complex_Matrix)
      renames Eigensystem_Body;

   function Eigenvalues (A : Complex_Matrix) return Real_Vector is
      Vectors : Complex_Matrix (A'Range (1), A'Range (2));
   begin
      return Result : Real_Vector (A'Range (2)) do
         Eigensystem (A, Result, Vectors);
      end return;
   end Eigenvalues;

   function Im_Body is
      new Generic_Arrays.Operator_Vector (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Im);

   function Im (X : Complex_Vector) return Real_Vector
      renames Im_Body;

   function Im_Body is
      new Generic_Arrays.Operator_Matrix (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Matrix,
         Im);

   function Im (X : Complex_Matrix) return Real_Matrix
      renames Im_Body;

   function Inverse_Body is
      new Generic_Arrays.Inverse (
         Complex,
         Complex_Matrix,
         One => (Re => 1.0, Im => 0.0));

   function Inverse (A : Complex_Matrix) return Complex_Matrix
      renames Inverse_Body;

   function Modulus_Body is
      new Generic_Arrays.Operator_Vector (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Modulus);

   function Modulus (X : Complex_Vector) return Real_Vector
      renames Modulus_Body;

   function Modulus_Body is
      new Generic_Arrays.Operator_Matrix (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Matrix,
         Modulus);

   function Modulus (X : Complex_Matrix) return Real_Matrix
      renames Modulus_Body;

   function Re_Body is
      new Generic_Arrays.Operator_Vector (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Re);

   function Re (X : Complex_Vector) return Real_Vector
      renames Re_Body;

   function Re_Body is
      new Generic_Arrays.Operator_Matrix (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Matrix,
         Re);

   function Re (X : Complex_Matrix) return Real_Matrix
      renames Re_Body;

   procedure Set_Im_Body is
      new Generic_Arrays.Apply_Vector (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Set_Im);

   procedure Set_Im (X : in out Complex_Vector; Im : Real_Vector)
      renames Set_Im_Body;

   procedure Set_Im_Body is
      new Generic_Arrays.Apply_Matrix (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Matrix,
         Set_Im);

   procedure Set_Im (X : in out Complex_Matrix; Im : Real_Matrix)
      renames Set_Im_Body;

   procedure Set_Re_Body is
      new Generic_Arrays.Apply_Vector (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Set_Re);

   procedure Set_Re (X : in out Complex_Vector; Re : Real_Vector)
      renames Set_Re_Body;

   procedure Set_Re_Body is
      new Generic_Arrays.Apply_Matrix (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Matrix,
         Set_Re);

   procedure Set_Re (X : in out Complex_Matrix; Re : Real_Matrix)
      renames Set_Re_Body;

   function Solve (A : Complex_Matrix; X : Complex_Vector)
      return Complex_Vector is
   begin
      return Inverse (A) * X;
   end Solve;

   function Solve (A, X : Complex_Matrix) return Complex_Matrix is
   begin
      return Inverse (A) * X;
   end Solve;

   function Transpose_Body is
      new Generic_Arrays.Transpose (Complex, Complex_Matrix);

   function Transpose (X : Complex_Matrix) return Complex_Matrix
      renames Transpose_Body;

   function Unit_Matrix_Body is
      new Generic_Arrays.Unit_Matrix (
         Complex,
         Complex_Matrix,
         Zero => (Re => 0.0, Im => 0.0),
         One => (Re => 1.0, Im => 0.0));

   function Unit_Matrix (
      Order : Positive;
      First_1, First_2 : Integer := 1) return Complex_Matrix
      renames Unit_Matrix_Body;

   function Unit_Vector_Body is
      new Generic_Arrays.Unit_Vector (
         Complex,
         Complex_Vector,
         Zero => (Re => 0.0, Im => 0.0),
         One => (Re => 1.0, Im => 0.0));

   function Unit_Vector (
      Index : Integer;
      Order : Positive;
      First : Integer := 1)
      return Complex_Vector
      renames Unit_Vector_Body;

   function abs_Body is
      new Generic_Arrays.Absolute (
         Complex,
         Complex_Vector,
         Real'Base,
         Zero => 0.0,
         Sqrt => Elementary_Functions.Sqrt);

   function "abs" (Right : Complex_Vector) return Real'Base
      renames abs_Body;

   function "+" (Right : Complex_Vector) return Complex_Vector is
   begin
      return Right;
   end "+";

   function add_Body is
      new Generic_Arrays.Operator_Vector_Vector (
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         "+");

   function "+" (Left, Right : Complex_Vector) return Complex_Vector
      renames add_Body;

   function "+" (Left : Real_Vector; Right : Complex_Vector)
      return Complex_Vector is
   begin
      return Right + Left;
   end "+";

   function add_Body is
      new Generic_Arrays.Operator_Vector_Vector (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Vector,
         "+");

   function "+" (Left : Complex_Vector; Right : Real_Vector)
      return Complex_Vector
      renames add_Body;

   function "+" (Right : Complex_Matrix) return Complex_Matrix is
   begin
      return Right;
   end "+";

   function add_Body is
      new Generic_Arrays.Operator_Matrix_Matrix (
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         "+");

   function "+" (Left, Right : Complex_Matrix) return Complex_Matrix
      renames add_Body;

   function "+" (Left : Real_Matrix; Right : Complex_Matrix)
      return Complex_Matrix is
   begin
      return Right + Left;
   end "+";

   function add_Body is
      new Generic_Arrays.Operator_Matrix_Matrix (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Matrix,
         "+");

   function "+" (Left : Complex_Matrix; Right : Real_Matrix)
      return Complex_Matrix
      renames add_Body;

   function neg_Body is
      new Generic_Arrays.Operator_Vector (
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         "-");

   function "-" (Right : Complex_Vector) return Complex_Vector
      renames neg_Body;

   function sub_Body is
      new Generic_Arrays.Operator_Vector_Vector (
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         "-");

   function "-" (Left, Right : Complex_Vector) return Complex_Vector
      renames sub_Body;

   function sub_Body is
      new Generic_Arrays.Operator_Vector_Vector (
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         "-");

   function "-" (Left : Real_Vector; Right : Complex_Vector)
      return Complex_Vector
      renames sub_Body;

   function sub_Body is
      new Generic_Arrays.Operator_Vector_Vector (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Vector,
         "-");

   function "-" (Left : Complex_Vector; Right : Real_Vector)
      return Complex_Vector
      renames sub_Body;

   function neg_Body is
      new Generic_Arrays.Operator_Matrix (
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         "-");

   function "-" (Right : Complex_Matrix) return Complex_Matrix
      renames neg_Body;

   function sub_Body is
      new Generic_Arrays.Operator_Matrix_Matrix (
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         "-");

   function "-" (Left, Right : Complex_Matrix) return Complex_Matrix
      renames sub_Body;

   function sub_Body is
      new Generic_Arrays.Operator_Matrix_Matrix (
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         "-");

   function "-" (Left : Real_Matrix; Right : Complex_Matrix)
      return Complex_Matrix
      renames sub_Body;

   function sub_Body is
      new Generic_Arrays.Operator_Matrix_Matrix (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Matrix,
         "-");

   function "-" (Left : Complex_Matrix; Right : Real_Matrix)
      return Complex_Matrix
      renames sub_Body;

   function mul_Body is
      new Generic_Arrays.Inner_Production (
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         Complex,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left, Right : Complex_Vector) return Complex
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Inner_Production (
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Vector,
         Complex,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Real_Vector; Right : Complex_Vector) return Complex
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Inner_Production (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Complex,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Complex_Vector; Right : Real_Vector) return Complex
      renames mul_Body;

   function "*" (Left : Complex; Right : Complex_Vector)
      return Complex_Vector is
   begin
      return Right * Left;
   end "*";

   function mul_Body is
      new Generic_Arrays.Operator_Vector_Param (
         Complex,
         Complex_Vector,
         Complex,
         Complex,
         Complex_Vector,
         "*");

   function "*" (Left : Complex_Vector; Right : Complex)
      return Complex_Vector
      renames mul_Body;

   function "*" (Left : Real'Base; Right : Complex_Vector)
      return Complex_Vector is
   begin
      return Right * Left;
   end "*";

   function mul_Body is
      new Generic_Arrays.Operator_Vector_Param (
         Complex,
         Complex_Vector,
         Real'Base,
         Complex,
         Complex_Vector,
         "*");

   function "*" (Left : Complex_Vector; Right : Real'Base)
      return Complex_Vector
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Matrix_Matrix (
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left, Right : Complex_Matrix) return Complex_Matrix
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Vector_Vector (
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         Complex,
         Complex_Matrix);

   function "*" (Left, Right : Complex_Vector) return Complex_Matrix
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Vector_Matrix (
         Complex,
         Complex_Vector,
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Vector,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Complex_Vector; Right : Complex_Matrix)
      return Complex_Vector
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Matrix_Vector (
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Complex_Matrix; Right : Complex_Vector)
      return Complex_Vector
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Matrix_Matrix (
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Matrix,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Real_Matrix; Right : Complex_Matrix)
      return Complex_Matrix
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Matrix_Matrix (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Matrix,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Complex_Matrix; Right : Real_Matrix)
      return Complex_Matrix
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Vector_Vector (
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Vector,
         Complex,
         Complex_Matrix);

   function "*" (Left : Real_Vector; Right : Complex_Vector)
      return Complex_Matrix
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Vector_Vector (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Matrix);

   function "*" (Left : Complex_Vector; Right : Real_Vector)
      return Complex_Matrix
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Vector_Matrix (
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Matrix,
         Complex,
         Complex_Vector,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Real_Vector; Right : Complex_Matrix)
      return Complex_Vector
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Vector_Matrix (
         Complex,
         Complex_Vector,
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Vector,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Complex_Vector; Right : Real_Matrix)
      return Complex_Vector
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Matrix_Vector (
         Real'Base,
         Real_Matrix,
         Complex,
         Complex_Vector,
         Complex,
         Complex_Vector,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Real_Matrix; Right : Complex_Vector)
      return Complex_Vector
      renames mul_Body;

   function mul_Body is
      new Generic_Arrays.Multiply_Matrix_Vector (
         Complex,
         Complex_Matrix,
         Real'Base,
         Real_Vector,
         Complex,
         Complex_Vector,
         Zero => (Re => 0.0, Im => 0.0));

   function "*" (Left : Complex_Matrix; Right : Real_Vector)
      return Complex_Vector
      renames mul_Body;

   function "*" (Left : Complex; Right : Complex_Matrix)
      return Complex_Matrix is
   begin
      return Right * Left;
   end "*";

   function mul_Body is
      new Generic_Arrays.Operator_Matrix_Param (
         Complex,
         Complex_Matrix,
         Complex,
         Complex,
         Complex_Matrix,
         "*");

   function "*" (Left : Complex_Matrix; Right : Complex)
      return Complex_Matrix
      renames mul_Body;

   function "*" (Left : Real'Base; Right : Complex_Matrix)
      return Complex_Matrix is
   begin
      return Right * Left;
   end "*";

   function mul_Body is
      new Generic_Arrays.Operator_Matrix_Param (
         Complex,
         Complex_Matrix,
         Real'Base,
         Complex,
         Complex_Matrix,
         "*");

   function "*" (Left : Complex_Matrix; Right : Real'Base)
      return Complex_Matrix
      renames mul_Body;

   function "/" (Left : Complex_Vector; Right : Complex)
      return Complex_Vector is
   begin
      return Left * (1.0 / Right);
   end "/";

   function "/" (Left : Complex_Vector; Right : Real'Base)
      return Complex_Vector is
   begin
      return Left * (1.0 / Right);
   end "/";

   function "/" (Left : Complex_Matrix; Right : Complex)
      return Complex_Matrix is
   begin
      return Left * (1.0 / Right);
   end "/";

   function "/" (Left : Complex_Matrix; Right : Real'Base)
      return Complex_Matrix is
   begin
      return Left * (1.0 / Right);
   end "/";

end Ada.Numerics.Generic_Complex_Arrays;
