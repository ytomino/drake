pragma License (Unrestricted);
--  implementation unit for Generic_Real_Arrays and Generic_Complex_Arrays
private package Ada.Numerics.Generic_Arrays is
   pragma Pure;

   --  vector selection, conversion, and composition operations

   generic
      type Number is private;
      type Vector is array (Integer range <>) of Number;
      type Parameter_Type is private;
      type Parameter_Vector is array (Integer range <>) of Parameter_Type;
      with procedure Apply (X : in out Number; Param : Parameter_Type);
   procedure Apply_Vector (X : in out Vector; Param : Parameter_Vector);

   --  vector arithmetic operations

   generic
      type Number is private;
      type Vector is array (Integer range <>) of Number;
      type Result_Type is private;
      type Result_Vector is array (Integer range <>) of Result_Type;
      with function Operator (Right : Number) return Result_Type;
   function Operator_Vector (Right : Vector) return Result_Vector;

   generic
      type Number is private;
      type Vector is array (Integer range <>) of Number;
      type Parameter_Type is private;
      type Result_Type is private;
      type Result_Vector is array (Integer range <>) of Result_Type;
      with function Operator (X : Number; Y : Parameter_Type)
         return Result_Type;
   function Operator_Vector_Param (X : Vector; Y : Parameter_Type)
      return Result_Vector;

   generic
      type Left_Type is private;
      type Left_Vector is array (Integer range <>) of Left_Type;
      type Right_Type is private;
      type Right_Vector is array (Integer range <>) of Right_Type;
      type Result_Type is private;
      type Result_Vector is array (Integer range <>) of Result_Type;
      with function Operator (Left : Left_Type; Right : Right_Type)
         return Result_Type;
   function Operator_Vector_Vector (Left : Left_Vector; Right : Right_Vector)
      return Result_Vector;

   generic
      type Number is private;
      type Vector is array (Integer range <>) of Number;
      type Parameter_Type is private;
      type Result_Type is private;
      type Result_Vector is array (Integer range <>) of Result_Type;
      with function Operator (X, Y : Number; Param : Parameter_Type)
         return Result_Type;
   function Operator_Vector_Vector_Param (X, Y : Vector; Z : Parameter_Type)
      return Result_Vector;

   generic
      type Number is private;
      type Vector is array (Integer range <>) of Number;
      type Result_Type is private;
      Zero : Result_Type;
      with function Sqrt (X : Result_Type) return Result_Type is <>;
      with function "abs" (Right : Number) return Result_Type is <>;
      with function "+" (Left, Right : Result_Type) return Result_Type is <>;
      with function "*" (Left, Right : Result_Type) return Result_Type is <>;
   function Absolute (Right : Vector) return Result_Type;

   generic
      type Left_Type is private;
      type Left_Vector is array (Integer range <>) of Left_Type;
      type Right_Type is private;
      type Right_Vector is array (Integer range <>) of Right_Type;
      type Result_Type is private;
      Zero : Result_Type;
      with function "+" (Left, Right : Result_Type) return Result_Type is <>;
      with function "*" (Left : Left_Type; Right : Right_Type)
         return Result_Type is <>;
   function Inner_Production (Left : Left_Vector; Right : Right_Vector)
      return Result_Type;

   --  other vector operations

   generic
      type Number is private;
      type Vector is array (Integer range <>) of Number;
      Zero : Number;
      One : Number;
   function Unit_Vector (
      Index : Integer;
      Order : Positive;
      First : Integer := 1)
      return Vector;

   --  matrix selection, conversion, and composition operations

   generic
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
      type Parameter_Type is private;
      type Parameter_Matrix is
         array (Integer range <>, Integer range <>) of Parameter_Type;
      with procedure Apply (X : in out Number; Param : Parameter_Type);
   procedure Apply_Matrix (X : in out Matrix; Param : Parameter_Matrix);

   --  matrix arithmetic operations

   generic
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
      type Result_Type is private;
      type Result_Matrix is
         array (Integer range <>, Integer range <>) of Result_Type;
      with function Operator (Right : Number) return Result_Type;
   function Operator_Matrix (Right : Matrix) return Result_Matrix;

   generic
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
      type Parameter_Type is private;
      type Result_Type is private;
      type Result_Matrix is
         array (Integer range <>, Integer range <>) of Result_Type;
      with function Operator (X : Number; Y : Parameter_Type)
         return Result_Type;
   function Operator_Matrix_Param (X : Matrix; Y : Parameter_Type)
      return Result_Matrix;

   generic
      type Left_Type is private;
      type Left_Matrix is
         array (Integer range <>, Integer range <>) of Left_Type;
      type Right_Type is private;
      type Right_Matrix is
         array (Integer range <>, Integer range <>) of Right_Type;
      type Result_Type is private;
      type Result_Matrix is
         array (Integer range <>, Integer range <>) of Result_Type;
      with function Operator (Left : Left_Type; Right : Right_Type)
         return Result_Type;
   function Operator_Matrix_Matrix (Left : Left_Matrix; Right : Right_Matrix)
      return Result_Matrix;

   generic
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
      type Parameter_Type is private;
      type Result_Type is private;
      type Result_Matrix is
         array (Integer range <>, Integer range <>) of Result_Type;
      with function Operator (X, Y : Number; Z : Parameter_Type)
         return Result_Type;
   function Operator_Matrix_Matrix_Param (X, Y : Matrix; Z : Parameter_Type)
      return Result_Matrix;

   generic
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
   function Transpose (X : Matrix) return Matrix;

   generic
      type Left_Type is private;
      type Left_Matrix is
         array (Integer range <>, Integer range <>) of Left_Type;
      type Right_Type is private;
      type Right_Matrix is
         array (Integer range <>, Integer range <>) of Right_Type;
      type Result_Type is private;
      type Result_Matrix is
         array (Integer range <>, Integer range <>) of Result_Type;
      Zero : Result_Type;
      with function "+" (Left, Right : Result_Type) return Result_Type is <>;
      with function "*" (Left : Left_Type; Right : Right_Type)
         return Result_Type is <>;
   function Multiply_Matrix_Matrix (Left : Left_Matrix; Right : Right_Matrix)
      return Result_Matrix;

   generic
      type Left_Type is private;
      type Left_Vector is array (Integer range <>) of Left_Type;
      type Right_Type is private;
      type Right_Vector is array (Integer range <>) of Right_Type;
      type Result_Type is private;
      type Result_Matrix is
         array (Integer range <>, Integer range <>) of Result_Type;
      with function "*" (Left : Left_Type; Right : Right_Type)
         return Result_Type is <>;
   function Multiply_Vector_Vector (Left : Left_Vector; Right : Right_Vector)
      return Result_Matrix;

   generic
      type Left_Type is private;
      type Left_Vector is array (Integer range <>) of Left_Type;
      type Right_Type is private;
      type Right_Matrix is
         array (Integer range <>, Integer range <>) of Right_Type;
      type Result_Type is private;
      type Result_Vector is array (Integer range <>) of Result_Type;
      Zero : Result_Type;
      with function "+" (Left, Right : Result_Type) return Result_Type is <>;
      with function "*" (Left : Left_Type; Right : Right_Type)
         return Result_Type is <>;
   function Multiply_Vector_Matrix (Left : Left_Vector; Right : Right_Matrix)
      return Result_Vector;

   generic
      type Left_Type is private;
      type Left_Matrix is
         array (Integer range <>, Integer range <>) of Left_Type;
      type Right_Type is private;
      type Right_Vector is array (Integer range <>) of Right_Type;
      type Result_Type is private;
      type Result_Vector is array (Integer range <>) of Result_Type;
      Zero : Result_Type;
      with function "+" (Left, Right : Result_Type) return Result_Type is <>;
      with function "*" (Left : Left_Type; Right : Right_Type)
         return Result_Type is <>;
   function Multiply_Matrix_Vector (Left : Left_Matrix; Right : Right_Vector)
      return Result_Vector;

   --  matrix inversion and related operations

   generic
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
   function Minor (A : Matrix; I, J : Integer) return Matrix;

   generic
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
      One : Number;
      with function Minor (A : Matrix; I, J : Integer) return Matrix is <>;
      with function Determinant (A : Matrix) return Number is <>;
      with function "-" (Right : Number) return Number is <>;
      with function "*" (Left, Right : Number) return Number is <>;
      with function "/" (Left, Right : Number) return Number is <>;
   function Inverse (A : Matrix) return Matrix;

   generic
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
      Zero : Number;
      One : Number;
      with function Minor (A : Matrix; I, J : Integer) return Matrix is <>;
      with function "+" (Left, Right : Number) return Number is <>;
      with function "-" (Right : Number) return Number is <>;
      with function "-" (Left, Right : Number) return Number is <>;
      with function "*" (Left, Right : Number) return Number is <>;
   function Determinant (A : Matrix) return Number;

   --  eigenvalues and vectors of a hermitian matrix

   generic
      type Real is private;
      type Real_Vector is array (Integer range <>) of Real;
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
      Zero : Number;
      One : Number;
      Two : Number;
      with function Sqrt (X : Number) return Number is <>;
      with function Is_Minus (X : Number) return Boolean is <>;
      with function Is_Small (X : Number) return Boolean is <>;
      with function To_Real (X : Number) return Real is <>;
      with function "+" (Left, Right : Number) return Number is <>;
      with function "-" (Right : Number) return Number is <>;
      with function "-" (Left, Right : Number) return Number is <>;
      with function "*" (Left, Right : Number) return Number is <>;
      with function "/" (Left, Right : Number) return Number is <>;
   procedure Eigensystem (
      A : Matrix;
      Values : out Real_Vector;
      Vectors : out Matrix);

   --  other matrix operations

   generic
      type Number is private;
      type Matrix is array (Integer range <>, Integer range <>) of Number;
      Zero : Number;
      One : Number;
   function Unit_Matrix (Order : Positive; First_1, First_2 : Integer := 1)
      return Matrix;

end Ada.Numerics.Generic_Arrays;
