--  reference: Netgen's Eigensystem routine.
package body Ada.Numerics.Generic_Arrays is
   pragma Suppress (All_Checks);

   function Operator_Vector (Right : Vector) return Result_Vector is
   begin
      return Result : Result_Vector (Right'Range) do
         for I in Result'Range loop
            Result (I) := Operator (Right (I));
         end loop;
      end return;
   end Operator_Vector;

   function Operator_Matrix (Right : Matrix) return Result_Matrix is
   begin
      return Result : Result_Matrix (Right'Range (1), Right'Range (2)) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               Result (I, J) := Operator (Right (I, J));
            end loop;
         end loop;
      end return;
   end Operator_Matrix;

   function Operator_Vector_Param (X : Vector; Y : Parameter_Type)
      return Result_Vector is
   begin
      return Result : Result_Vector (X'Range) do
         for I in Result'Range loop
            Result (I) := Operator (X (I), Y);
         end loop;
      end return;
   end Operator_Vector_Param;

   function Operator_Matrix_Param (X : Matrix; Y : Parameter_Type)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (X'Range (1), X'Range (2)) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               Result (I, J) := Operator (X (I, J), Y);
            end loop;
         end loop;
      end return;
   end Operator_Matrix_Param;

   function Operator_Vector_Vector (Left : Left_Vector; Right : Right_Vector)
      return Result_Vector is
   begin
      return Result : Result_Vector (Left'Range) do
         for I in Result'Range loop
            Result (I) := Operator (Left (I),
                                    Right (Right'First - Left'First + I));
         end loop;
      end return;
   end Operator_Vector_Vector;

   function Operator_Matrix_Matrix (Left : Left_Matrix; Right : Right_Matrix)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (Left'Range (1), Left'Range (2)) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               Result (I, J) := Operator (
                  Left (I, J),
                  Right (Right'First (1) - Left'First (1) + I,
                     Right'First (2) - Left'First (2) + J));
            end loop;
         end loop;
      end return;
   end Operator_Matrix_Matrix;

   function Operator_Vector_Vector_Param (X, Y : Vector; Z : Parameter_Type)
      return Result_Vector is
   begin
      return Result : Result_Vector (X'Range) do
         for I in Result'Range loop
            Result (I) := Operator (X (I),
                                    Y (Y'First - X'First + I),
                                    Z);
         end loop;
      end return;
   end Operator_Vector_Vector_Param;

   function Operator_Matrix_Matrix_Param (X, Y : Matrix; Z : Parameter_Type)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (X'Range (1), X'Range (2)) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               Result (I, J) := Operator (X (I, J),
                                          Y (Y'First (1) - X'First (1) + I,
                                             Y'First (2) - X'First (2) + J),
                                          Z);
            end loop;
         end loop;
      end return;
   end Operator_Matrix_Matrix_Param;

   procedure Apply_Vector (X : in out Vector; Param : Parameter_Vector) is
   begin
      for I in X'Range loop
         Apply (X (I), Param (Param'First - X'First + I));
      end loop;
   end Apply_Vector;

   procedure Apply_Matrix (X : in out Matrix; Param : Parameter_Matrix) is
   begin
      for I in X'Range (1) loop
         for J in X'Range (1) loop
            Apply (X (I, J), Param (Param'First (1) - X'First (1) + I,
                                    Param'First (2) - X'First (2) + J));
         end loop;
      end loop;
   end Apply_Matrix;

   function Minor (A : Matrix; I, J : Integer) return Matrix is
   begin
      return Result : Matrix (A'First (1) + 1 .. A'Last (1),
                              A'First (2) + 1 .. A'Last (2))
      do
         for I2 in Result'First (1) .. I loop
            for J2 in Result'First (2) .. J loop
               Result (I2, J2) := A (I2 - 1, J2 - 1);
            end loop;
            for J2 in J + 1 .. Result'Last (2) loop
               Result (I2, J2) := A (I2 - 1, J2);
            end loop;
         end loop;
         for I2 in I + 1 .. Result'Last (1) loop
            for J2 in Result'First (2) .. J loop
               Result (I2, J2) := A (I2, J2 - 1);
            end loop;
            for J2 in J + 1 .. Result'Last (2) loop
               Result (I2, J2) := A (I2, J2);
            end loop;
         end loop;
      end return;
   end Minor;

   function Determinant (A : Matrix) return Number is
   begin
      case A'Length (1) is
         when 1 =>
            return A (A'First (1), A'First (2));
         when 2 =>
            return A (A'First (1), A'First (2)) * A (A'Last (1), A'Last (2))
               - A (A'First (1), A'Last (2)) * A (A'Last (1), A'First (2));
         when others =>
            declare
               Result : Number := Zero;
               Sign : Number := One;
            begin
               for X in A'Range (1) loop
                  Result := Result
                     + Sign * A (X, A'First (2))
                        * Determinant (Minor (A, X, A'First (2)));
                  Sign := -Sign;
               end loop;
               return Result;
            end;
      end case;
   end Determinant;

   procedure Eigensystem (
      A : Matrix;
      Values : out Real_Vector;
      Vectors : out Matrix)
   is
      mat1 : Matrix
         renames A;
      lami : Real_Vector
         renames Values;
      eigenvecs : Matrix
         renames Vectors;
      n : constant Integer := A'Length (1);
      mat : Matrix := mat1;
      a11, a12, a22, p, q, y11, y12, y21, y22, y, v1, v2 : Number;
   begin
      for i in eigenvecs'Range (1) loop
         for j in eigenvecs'Range (2) loop
            eigenvecs (i, j) := Zero;
         end loop;
      end loop;
      for i in 0 .. eigenvecs'Length (1) - 1 loop
         eigenvecs (eigenvecs'First (1) + i, eigenvecs'First (2) + i) := One;
      end loop;
      for l in 0 .. 99 loop
         for i in 0 .. n - 1 loop
            for j in 0 .. i - 1 loop
               --  find eigensystem of a(i-j,i-j)
               a11 := mat (mat'First (1) + i, mat'First (2) + i);
               a12 := mat (mat'First (1) + i, mat'First (2) + j);
               a22 := mat (mat'First (1) + j, mat'First (2) + j);
               if Is_Small (a12 * a12) then -- 1.0e-32 * abs (a11 * a22)
                  null; -- continue
               else
                  p := (a22 - a11) / Two;
                  q := a12;
                  --  compute eigenvectors:
                  y11 := a12;
                  if not Is_Minus (p) then
                     y12 := p + Sqrt (p * p + q * q);
                  else
                     y12 := -q * q / (p - Sqrt (p * p + q * q));
                  end if;
                  y := Sqrt (y11 * y11 + y12 * y12);
                  y11 := y11 / y;
                  y12 := y12 / y;
                  y21 := a12;
                  if Is_Minus (p) then
                     y22 := p - Sqrt (p * p + q * q);
                  else
                     y22 := -q * q / (p + Sqrt (p * p + q * q));
                  end if;
                  y := Sqrt (y21 * y21 + y22 * y22);
                  y21 := y21 / y;
                  y22 := y22 / y;
                  --  V^T A V = V^T G^{-1} . (G^T A G) . G^{-1} V
                  for k in 0 .. n - 1 loop
                     v1 := mat (mat'First (1) + k, mat'First (2) + i);
                     v2 := mat (mat'First (1) + k, mat'First (2) + j);
                     mat (mat'First (1) + k, mat'First (2) + i) :=
                        v1 * y11 + v2 * y12;
                     mat (mat'First (1) + k, mat'First (2) + j) :=
                        v1 * y21 + v2 * y22;
                  end loop;
                  for k in 0 .. n - 1 loop
                     v1 := mat (mat'First (1) + i, mat'First (2) + k);
                     v2 := mat (mat'First (1) + j, mat'First (2) + k);
                     mat (mat'First (1) + i, mat'First (2) + k) :=
                        v1 * y11 + v2 * y12;
                     mat (mat'First (1) + j, mat'First (2) + k) :=
                        v1 * y21 + v2 * y22;
                  end loop;
                  mat (mat'First (1) + i, mat'First (2) + j) := Zero;
                  mat (mat'First (1) + j, mat'First (2) + i) := Zero;
                  for k in 0 .. n - 1 loop
                     v1 := eigenvecs (eigenvecs'First (1) + i,
                                      eigenvecs'First (2) + k);
                     v2 := eigenvecs (eigenvecs'First (1) + j,
                                      eigenvecs'First (2) + k);
                     eigenvecs (eigenvecs'First (1) + i,
                                eigenvecs'First (2) + k) :=
                        v1 * y11 + v2 * y12;
                     eigenvecs (eigenvecs'First (1) + j,
                                eigenvecs'First (2) + k) :=
                        v1 * y21 + v2 * y22;
                  end loop;
               end if;
            end loop;
         end loop;
      end loop;
      for i in lami'Range loop
         lami (i) := To_Real (mat (mat'First (1) - lami'First + i,
                                   mat'First (2) - lami'First + i));
      end loop;
   end Eigensystem;

   function Inverse (A : Matrix) return Matrix is
      detA : constant Number := Determinant (A);
      Master_Sign : Number := One;
      Sign : Number;
   begin
      return Result : Matrix (A'Range (1), A'Range (2)) do
         for I in Result'Range (1) loop
            Sign := Master_Sign;
            for J in Result'Range (2) loop
               Result (I, J) := Sign * Determinant (
                  Minor (A, Result'First (1) - Result'First (2) + J,
                            Result'First (2) - Result'First (1) + I)) / detA;
               Sign := -Sign;
            end loop;
            Master_Sign := -Master_Sign;
         end loop;
      end return;
   end Inverse;

   function Transpose (X : Matrix) return Matrix is
   begin
      return Result : Matrix (X'Range (2), X'Range (1)) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               Result (I, J) := X (J, I);
            end loop;
         end loop;
      end return;
   end Transpose;

   function Unit_Matrix (
      Order : Positive;
      First_1, First_2 : Integer := 1)
      return Matrix is
   begin
      return Result : Matrix (First_1 .. First_1 + Order - 1,
                              First_2 .. First_2 + Order - 1) :=
         (others => (others => Zero))
      do
         for I in 0 .. Order - 1 loop
            Result (First_1 + I, First_2 + I) := One;
         end loop;
      end return;
   end Unit_Matrix;

   function Unit_Vector (
      Index : Integer;
      Order : Positive;
      First : Integer := 1)
      return Vector is
   begin
      return Result : Vector (First .. First + Order - 1) :=
         (others => Zero)
      do
         Result (Index) := One;
      end return;
   end Unit_Vector;

   function Absolute (Right : Vector) return Result_Type is
      Result : Result_Type := Zero;
   begin
      for I in Right'Range loop
         declare
            X : constant Result_Type := abs Right (I);
         begin
            Result := Result + X * X;
         end;
      end loop;
      return Sqrt (Result);
   end Absolute;

   function Inner_Production (Left : Left_Vector; Right : Right_Vector)
      return Result_Type
   is
      Result : Result_Type := Zero;
   begin
      for I in Left'Range loop
         Result := Result + Left (I) * Right (Right'First - Left'First + I);
      end loop;
      return Result;
   end Inner_Production;

   function Multiply_Matrix_Matrix (Left : Left_Matrix; Right : Right_Matrix)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (Left'Range (1), Right'Range (2)) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               declare
                  Z : Result_Type := Zero;
               begin
                  for K in Left'Range (2) loop
                     Z := Z
                        + Left (I, K) *
                           Right (Right'First (1) - Left'First (2) + K, J);
                  end loop;
                  Result (I, J) := Z;
               end;
            end loop;
         end loop;
      end return;
   end Multiply_Matrix_Matrix;

   function Multiply_Vector_Vector (Left : Left_Vector; Right : Right_Vector)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (Left'Range, Right'Range) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               Result (I, J) := Left (I) * Right (J);
            end loop;
         end loop;
      end return;
   end Multiply_Vector_Vector;

   function Multiply_Vector_Matrix (Left : Left_Vector; Right : Right_Matrix)
      return Result_Vector is
   begin
      return Result : Result_Vector (Right'Range (2)) do
         for J in Result'Range loop
            declare
               Z : Result_Type := Zero;
            begin
               for K in Left'Range loop
                  Z := Z
                     + Left (K) * Right (Right'First (1) - Left'First + K, J);
               end loop;
               Result (J) := Z;
            end;
         end loop;
      end return;
   end Multiply_Vector_Matrix;

   function Multiply_Matrix_Vector (Left : Left_Matrix; Right : Right_Vector)
      return Result_Vector is
   begin
      return Result : Result_Vector (Left'Range (1)) do
         for I in Result'Range loop
            declare
               Z : Result_Type := Zero;
            begin
               for K in Left'Range (2) loop
                  Z := Z
                     + Left (I, K) * Right (Right'First - Left'First (2) + K);
               end loop;
               Result (I) := Z;
            end;
         end loop;
      end return;
   end Multiply_Matrix_Vector;

end Ada.Numerics.Generic_Arrays;
