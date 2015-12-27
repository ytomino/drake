with Ada.Numerics.Generic_Arrays;
package body Ada.Numerics.Generic_Complex_Arrays is

   package Elementary_Functions is

      subtype Float_Type is Real_Arrays.Real;

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

   function Minor is new Generic_Arrays.Minor (Complex, Complex_Matrix);
      --  for Inverse and Determinant

   --  implementation

   function Re (X : Complex_Vector) return Real_Vector is
      function Re_Body is
         new Generic_Arrays.Operator_Vector (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Re);
   begin
      return Re_Body (X);
   end Re;

   function Im (X : Complex_Vector) return Real_Vector is
      function Im_Body is
         new Generic_Arrays.Operator_Vector (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Im);
   begin
      return Im_Body (X);
   end Im;

   procedure Set_Re (X : in out Complex_Vector; Re : Real_Vector) is
      procedure Set_Re_Body is
         new Generic_Arrays.Apply_Vector (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Set_Re);
   begin
      Set_Re_Body (X, Re);
   end Set_Re;

   procedure Set_Im (X : in out Complex_Vector; Im : Real_Vector) is
      procedure Set_Im_Body is
         new Generic_Arrays.Apply_Vector (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Set_Im);
   begin
      Set_Im_Body (X, Im);
   end Set_Im;

   function Compose_From_Cartesian (Re : Real_Vector) return Complex_Vector is
      function Compose_From_Cartesian_Body is
         new Generic_Arrays.Operator_Vector (
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Vector,
            Compose_From_Cartesian);
   begin
      return Compose_From_Cartesian_Body (Re);
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Re, Im : Real_Vector)
      return Complex_Vector
   is
      function Compose_From_Cartesian_Body is
         new Generic_Arrays.Operator_Vector_Vector (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Vector,
            Compose_From_Cartesian);
   begin
      return Compose_From_Cartesian_Body (Re, Im);
   end Compose_From_Cartesian;

   function Modulus (X : Complex_Vector) return Real_Vector is
      function Modulus_Body is
         new Generic_Arrays.Operator_Vector (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Modulus);
   begin
      return Modulus_Body (X);
   end Modulus;

   function Argument (X : Complex_Vector) return Real_Vector is
      function Argument_Body is
         new Generic_Arrays.Operator_Vector (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Argument);
   begin
      return Argument_Body (X);
   end Argument;

   function Argument (X : Complex_Vector; Cycle : Real'Base)
      return Real_Vector
   is
      function Argument_Body is
         new Generic_Arrays.Operator_Vector_Param (
            Complex,
            Complex_Vector,
            Real'Base,
            Real'Base,
            Real_Vector,
            Argument);
   begin
      return Argument_Body (X, Cycle);
   end Argument;

   function Compose_From_Polar (Modulus, Argument : Real_Vector)
      return Complex_Vector
   is
      function Compose_From_Polar_Body is
         new Generic_Arrays.Operator_Vector_Vector (
            Real'Base,
            Real_Vector,
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Vector,
            Compose_From_Polar);
   begin
      return Compose_From_Polar_Body (Modulus, Argument);
   end Compose_From_Polar;

   function Compose_From_Polar (
      Modulus, Argument : Real_Vector;
      Cycle : Real'Base)
      return Complex_Vector
   is
      function Compose_From_Polar_Body is
         new Generic_Arrays.Operator_Vector_Vector_Param (
            Real'Base,
            Real_Vector,
            Real'Base,
            Complex,
            Complex_Vector,
            Compose_From_Polar);
   begin
      return Compose_From_Polar_Body (Modulus, Argument, Cycle);
   end Compose_From_Polar;

   function "+" (Right : Complex_Vector) return Complex_Vector is
   begin
      return Right;
   end "+";

   function "-" (Right : Complex_Vector) return Complex_Vector is
      function neg_Body is
         new Generic_Arrays.Operator_Vector (
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            "-");
   begin
      return neg_Body (Right);
   end "-";

   function Conjugate (X : Complex_Vector) return Complex_Vector is
      function Conjugate_Body is
         new Generic_Arrays.Operator_Vector (
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            Conjugate);
   begin
      return Conjugate_Body (X);
   end Conjugate;

   function "+" (Left, Right : Complex_Vector) return Complex_Vector is
      function add_Body is
         new Generic_Arrays.Operator_Vector_Vector (
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            "+");
   begin
      return add_Body (Left, Right);
   end "+";

   function "-" (Left, Right : Complex_Vector) return Complex_Vector is
      function sub_Body is
         new Generic_Arrays.Operator_Vector_Vector (
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            "-");
   begin
      return sub_Body (Left, Right);
   end "-";

   function "*" (Left, Right : Complex_Vector) return Complex is
      function mul_Body is
         new Generic_Arrays.Inner_Production (
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            Complex,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "abs" (Right : Complex_Vector) return Real'Base is
      function abs_Body is
         new Generic_Arrays.Absolute (
            Complex,
            Complex_Vector,
            Real'Base,
            Zero => 0.0,
            Sqrt => Elementary_Functions.Sqrt);
   begin
      return abs_Body (Right);
   end "abs";

   function "+" (Left : Real_Vector; Right : Complex_Vector)
      return Complex_Vector is
   begin
      return Right + Left;
   end "+";

   function "+" (Left : Complex_Vector; Right : Real_Vector)
      return Complex_Vector
   is
      function add_Body is
         new Generic_Arrays.Operator_Vector_Vector (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Vector,
            "+");
   begin
      return add_Body (Left, Right);
   end "+";

   function "-" (Left : Real_Vector; Right : Complex_Vector)
      return Complex_Vector
   is
      function sub_Body is
         new Generic_Arrays.Operator_Vector_Vector (
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            "-");
   begin
      return sub_Body (Left, Right);
   end "-";

   function "-" (Left : Complex_Vector; Right : Real_Vector)
      return Complex_Vector
   is
      function sub_Body is
         new Generic_Arrays.Operator_Vector_Vector (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Vector,
            "-");
   begin
      return sub_Body (Left, Right);
   end "-";

   function "*" (Left : Real_Vector; Right : Complex_Vector) return Complex is
      function mul_Body is
         new Generic_Arrays.Inner_Production (
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Vector,
            Complex,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Complex_Vector; Right : Real_Vector) return Complex is
      function mul_Body is
         new Generic_Arrays.Inner_Production (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Complex,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Complex; Right : Complex_Vector)
      return Complex_Vector is
   begin
      return Right * Left;
   end "*";

   function "*" (Left : Complex_Vector; Right : Complex)
      return Complex_Vector
   is
      function mul_Body is
         new Generic_Arrays.Operator_Vector_Param (
            Complex,
            Complex_Vector,
            Complex,
            Complex,
            Complex_Vector,
            "*");
   begin
      return mul_Body (Left, Right);
   end "*";

   function "/" (Left : Complex_Vector; Right : Complex)
      return Complex_Vector is
   begin
      return Left * (1.0 / Right);
   end "/";

   function "*" (Left : Real'Base; Right : Complex_Vector)
      return Complex_Vector is
   begin
      return Right * Left;
   end "*";

   function "*" (Left : Complex_Vector; Right : Real'Base)
      return Complex_Vector
   is
      function mul_Body is
         new Generic_Arrays.Operator_Vector_Param (
            Complex,
            Complex_Vector,
            Real'Base,
            Complex,
            Complex_Vector,
            "*");
   begin
      return mul_Body (Left, Right);
   end "*";

   function "/" (Left : Complex_Vector; Right : Real'Base)
      return Complex_Vector is
   begin
      return Left * (1.0 / Right);
   end "/";

   function Unit_Vector (
      Index : Integer;
      Order : Positive;
      First : Integer := 1)
      return Complex_Vector
   is
      function Unit_Vector_Body is
         new Generic_Arrays.Unit_Vector (
            Complex,
            Complex_Vector,
            Zero => (Re => 0.0, Im => 0.0),
            One => (Re => 1.0, Im => 0.0));
   begin
      return Unit_Vector_Body (Index, Order, First);
   end Unit_Vector;

   function Re (X : Complex_Matrix) return Real_Matrix is
      function Re_Body is
         new Generic_Arrays.Operator_Matrix (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Matrix,
            Re);
   begin
      return Re_Body (X);
   end Re;

   function Im (X : Complex_Matrix) return Real_Matrix is
      function Im_Body is
         new Generic_Arrays.Operator_Matrix (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Matrix,
            Im);
   begin
      return Im_Body (X);
   end Im;

   procedure Set_Re (X : in out Complex_Matrix; Re : Real_Matrix) is
      procedure Set_Re_Body is
         new Generic_Arrays.Apply_Matrix (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Matrix,
            Set_Re);
   begin
      Set_Re_Body (X, Re);
   end Set_Re;

   procedure Set_Im (X : in out Complex_Matrix; Im : Real_Matrix) is
      procedure Set_Im_Body is
         new Generic_Arrays.Apply_Matrix (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Matrix,
            Set_Im);
   begin
      Set_Im_Body (X, Im);
   end Set_Im;

   function Compose_From_Cartesian (Re : Real_Matrix) return Complex_Matrix is
      function Compose_From_Cartesian_Body is
         new Generic_Arrays.Operator_Matrix (
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Matrix,
            Compose_From_Cartesian);
   begin
      return Compose_From_Cartesian_Body (Re);
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Re, Im : Real_Matrix)
      return Complex_Matrix
   is
      function Compose_From_Cartesian_Body is
         new Generic_Arrays.Operator_Matrix_Matrix (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Matrix,
            Compose_From_Cartesian);
   begin
      return Compose_From_Cartesian_Body (Re, Im);
   end Compose_From_Cartesian;

   function Modulus (X : Complex_Matrix) return Real_Matrix is
      function Modulus_Body is
         new Generic_Arrays.Operator_Matrix (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Matrix,
            Modulus);
   begin
      return Modulus_Body (X);
   end Modulus;

   function Argument (X : Complex_Matrix) return Real_Matrix is
      function Argument_Body is
         new Generic_Arrays.Operator_Matrix (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Matrix,
            Argument);
   begin
      return Argument_Body (X);
   end Argument;

   function Argument (X : Complex_Matrix; Cycle : Real'Base)
      return Real_Matrix
   is
      function Argument_Body is
         new Generic_Arrays.Operator_Matrix_Param (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real'Base,
            Real_Matrix,
            Argument);
   begin
      return Argument_Body (X, Cycle);
   end Argument;

   function Compose_From_Polar (Modulus, Argument : Real_Matrix)
      return Complex_Matrix
   is
      function Compose_From_Polar_Body is
         new Generic_Arrays.Operator_Matrix_Matrix (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Matrix,
            Compose_From_Polar);
   begin
      return Compose_From_Polar_Body (Modulus, Argument);
   end Compose_From_Polar;

   function Compose_From_Polar (
      Modulus, Argument : Real_Matrix;
      Cycle : Real'Base)
      return Complex_Matrix
   is
      function Compose_From_Polar_Body is
         new Generic_Arrays.Operator_Matrix_Matrix_Param (
            Real'Base,
            Real_Matrix,
            Real'Base,
            Complex,
            Complex_Matrix,
            Compose_From_Polar);
   begin
      return Compose_From_Polar_Body (Modulus, Argument, Cycle);
   end Compose_From_Polar;

   function "+" (Right : Complex_Matrix) return Complex_Matrix is
   begin
      return Right;
   end "+";

   function "-" (Right : Complex_Matrix) return Complex_Matrix is
      function neg_Body is
         new Generic_Arrays.Operator_Matrix (
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            "-");
   begin
      return neg_Body (Right);
   end "-";

   function Conjugate (X : Complex_Matrix) return Complex_Matrix is
      function Conjugate_Body is
         new Generic_Arrays.Operator_Matrix (
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            Conjugate);
   begin
      return Conjugate_Body (X);
   end Conjugate;

   function Transpose (X : Complex_Matrix) return Complex_Matrix is
      function Transpose_Body is
         new Generic_Arrays.Transpose (Complex, Complex_Matrix);
   begin
      return Transpose_Body (X);
   end Transpose;

   function "+" (Left, Right : Complex_Matrix) return Complex_Matrix is
      function add_Body is
         new Generic_Arrays.Operator_Matrix_Matrix (
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            "+");
   begin
      return add_Body (Left, Right);
   end "+";

   function "-" (Left, Right : Complex_Matrix) return Complex_Matrix is
      function sub_Body is
         new Generic_Arrays.Operator_Matrix_Matrix (
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            "-");
   begin
      return sub_Body (Left, Right);
   end "-";

   function "*" (Left, Right : Complex_Matrix) return Complex_Matrix is
      function mul_Body is
         new Generic_Arrays.Multiply_Matrix_Matrix (
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left, Right : Complex_Vector) return Complex_Matrix is
      function mul_Body is
         new Generic_Arrays.Multiply_Vector_Vector (
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            Complex,
            Complex_Matrix);
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Complex_Vector; Right : Complex_Matrix)
      return Complex_Vector
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Vector_Matrix (
            Complex,
            Complex_Vector,
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Vector,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Complex_Matrix; Right : Complex_Vector)
      return Complex_Vector
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Matrix_Vector (
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "+" (Left : Real_Matrix; Right : Complex_Matrix)
      return Complex_Matrix is
   begin
      return Right + Left;
   end "+";

   function "+" (Left : Complex_Matrix; Right : Real_Matrix)
      return Complex_Matrix
   is
      function add_Body is
         new Generic_Arrays.Operator_Matrix_Matrix (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Matrix,
            "+");
   begin
      return add_Body (Left, Right);
   end "+";

   function "-" (Left : Real_Matrix; Right : Complex_Matrix)
      return Complex_Matrix
   is
      function sub_Body is
         new Generic_Arrays.Operator_Matrix_Matrix (
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            "-");
   begin
      return sub_Body (Left, Right);
   end "-";

   function "-" (Left : Complex_Matrix; Right : Real_Matrix)
      return Complex_Matrix
   is
      function sub_Body is
         new Generic_Arrays.Operator_Matrix_Matrix (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Matrix,
            "-");
   begin
      return sub_Body (Left, Right);
   end "-";

   function "*" (Left : Real_Matrix; Right : Complex_Matrix)
      return Complex_Matrix
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Matrix_Matrix (
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Matrix,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Complex_Matrix; Right : Real_Matrix)
      return Complex_Matrix
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Matrix_Matrix (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Matrix,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Real_Vector; Right : Complex_Vector)
      return Complex_Matrix
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Vector_Vector (
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Vector,
            Complex,
            Complex_Matrix);
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Complex_Vector; Right : Real_Vector)
      return Complex_Matrix
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Vector_Vector (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Matrix);
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Real_Vector; Right : Complex_Matrix)
      return Complex_Vector
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Vector_Matrix (
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Matrix,
            Complex,
            Complex_Vector,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Complex_Vector; Right : Real_Matrix)
      return Complex_Vector
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Vector_Matrix (
            Complex,
            Complex_Vector,
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Vector,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Real_Matrix; Right : Complex_Vector)
      return Complex_Vector
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Matrix_Vector (
            Real'Base,
            Real_Matrix,
            Complex,
            Complex_Vector,
            Complex,
            Complex_Vector,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Complex_Matrix; Right : Real_Vector)
      return Complex_Vector
   is
      function mul_Body is
         new Generic_Arrays.Multiply_Matrix_Vector (
            Complex,
            Complex_Matrix,
            Real'Base,
            Real_Vector,
            Complex,
            Complex_Vector,
            Zero => (Re => 0.0, Im => 0.0));
   begin
      return mul_Body (Left, Right);
   end "*";

   function "*" (Left : Complex; Right : Complex_Matrix)
      return Complex_Matrix is
   begin
      return Right * Left;
   end "*";

   function "*" (Left : Complex_Matrix; Right : Complex)
      return Complex_Matrix
   is
      function mul_Body is
         new Generic_Arrays.Operator_Matrix_Param (
            Complex,
            Complex_Matrix,
            Complex,
            Complex,
            Complex_Matrix,
            "*");
   begin
      return mul_Body (Left, Right);
   end "*";

   function "/" (Left : Complex_Matrix; Right : Complex)
      return Complex_Matrix is
   begin
      return Left * (1.0 / Right);
   end "/";

   function "*" (Left : Real'Base; Right : Complex_Matrix)
      return Complex_Matrix is
   begin
      return Right * Left;
   end "*";

   function "*" (Left : Complex_Matrix; Right : Real'Base)
      return Complex_Matrix
   is
      function mul_Body is
         new Generic_Arrays.Operator_Matrix_Param (
            Complex,
            Complex_Matrix,
            Real'Base,
            Complex,
            Complex_Matrix,
            "*");
   begin
      return mul_Body (Left, Right);
   end "*";

   function "/" (Left : Complex_Matrix; Right : Real'Base)
      return Complex_Matrix is
   begin
      return Left * (1.0 / Right);
   end "/";

   function Solve (A : Complex_Matrix; X : Complex_Vector)
      return Complex_Vector is
   begin
      return Inverse (A) * X;
   end Solve;

   function Solve (A, X : Complex_Matrix) return Complex_Matrix is
   begin
      return Inverse (A) * X;
   end Solve;

   function Inverse (A : Complex_Matrix) return Complex_Matrix is
      function Inverse_Body is
         new Generic_Arrays.Inverse (
            Complex,
            Complex_Matrix,
            One => (Re => 1.0, Im => 0.0));
   begin
      return Inverse_Body (A);
   end Inverse;

   function Determinant (A : Complex_Matrix) return Complex is
      function Determinant_Body is
         new Generic_Arrays.Determinant (
            Complex,
            Complex_Matrix,
            Zero => (Re => 0.0, Im => 0.0),
            One => (Re => 1.0, Im => 0.0));
   begin
      return Determinant_Body (A);
   end Determinant;

   function Eigenvalues (A : Complex_Matrix) return Real_Vector is
      Vectors : Complex_Matrix (A'Range (1), A'Range (2));
   begin
      return Result : Real_Vector (A'Range (2)) do
         Eigensystem (A, Result, Vectors);
      end return;
   end Eigenvalues;

   procedure Eigensystem (
      A : Complex_Matrix;
      Values : out Real_Vector;
      Vectors : out Complex_Matrix)
   is
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
   begin
      Eigensystem_Body (A, Values, Vectors);
   end Eigensystem;

   function Unit_Matrix (Order : Positive; First_1, First_2 : Integer := 1)
      return Complex_Matrix
   is
      function Unit_Matrix_Body is
         new Generic_Arrays.Unit_Matrix (
            Complex,
            Complex_Matrix,
            Zero => (Re => 0.0, Im => 0.0),
            One => (Re => 1.0, Im => 0.0));
   begin
      return Unit_Matrix_Body (Order, First_1, First_2);
   end Unit_Matrix;

end Ada.Numerics.Generic_Complex_Arrays;
