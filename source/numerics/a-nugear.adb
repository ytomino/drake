--  reference: Netgen's Eigensystem routine.
package body Ada.Numerics.Generic_Arrays is

   --  vector selection, conversion, and composition operations

   procedure Apply_Vector (X : in out Vector; Param : Parameter_Vector) is
      Length : constant Integer := X'Length;
   begin
      for I in 0 .. Length - 1 loop
         pragma Loop_Optimize (Vector);
         Apply (X (X'First + I), Param (Param'First + I));
      end loop;
   end Apply_Vector;

   --  vector arithmetic operations

   function Operator_Vector (Right : Vector) return Result_Vector is
   begin
      return Result : Result_Vector (Right'Range) do
         for I in Result'Range loop
            pragma Loop_Optimize (Vector);
            Result (I) := Operator (Right (I));
         end loop;
      end return;
   end Operator_Vector;

   function Operator_Vector_Param (X : Vector; Y : Parameter_Type)
      return Result_Vector is
   begin
      return Result : Result_Vector (X'Range) do
         for I in Result'Range loop
            pragma Loop_Optimize (Vector);
            Result (I) := Operator (X (I), Y);
         end loop;
      end return;
   end Operator_Vector_Param;

   function Operator_Vector_Vector (Left : Left_Vector; Right : Right_Vector)
      return Result_Vector is
   begin
      return Result : Result_Vector (Left'Range) do
         declare
            Length : constant Integer := Result'Length;
         begin
            for I in 0 .. Length - 1 loop
               pragma Loop_Optimize (Vector);
               Result (Result'First + I) :=
                  Operator (Left (Left'First + I), Right (Right'First + I));
            end loop;
         end;
      end return;
   end Operator_Vector_Vector;

   function Operator_Vector_Vector_Param (X, Y : Vector; Z : Parameter_Type)
      return Result_Vector is
   begin
      return Result : Result_Vector (X'Range) do
         declare
            Length : constant Integer := Result'Length;
         begin
            for I in 0 .. Length - 1 loop
               pragma Loop_Optimize (Vector);
               Result (Result'First + I) :=
                  Operator (X (X'First + I), Y (Y'First + I), Z);
            end loop;
         end;
      end return;
   end Operator_Vector_Vector_Param;

   function Absolute (Right : Vector) return Result_Type is
      Squared : Result_Type := Zero;
   begin
      for I in Right'Range loop
         pragma Loop_Optimize (Vector);
         declare
            X : constant Result_Type := abs Right (I);
         begin
            Squared := Squared + X * X;
         end;
      end loop;
      return Sqrt (Squared);
   end Absolute;

   function Inner_Production (Left : Left_Vector; Right : Right_Vector)
      return Result_Type
   is
      Result : Result_Type := Zero;
      Length : constant Integer := Left'Length;
   begin
      for I in 0 .. Length - 1 loop
         pragma Loop_Optimize (Vector);
         Result := Result + Left (Left'First + I) * Right (Right'First + I);
      end loop;
      return Result;
   end Inner_Production;

   --  other vector operations

   function Unit_Vector (
      Index : Integer;
      Order : Positive;
      First : Integer := 1)
      return Vector is
   begin
      return Result : Vector (First .. First + Order - 1) do
         for I in Result'Range loop
            pragma Loop_Optimize (Vector);
            Result (I) := Zero;
         end loop;
         Result (Index) := One;
      end return;
   end Unit_Vector;

   --  matrix selection, conversion, and composition operations

   procedure Apply_Matrix (X : in out Matrix; Param : Parameter_Matrix) is
      Length_1 : constant Integer := X'Length (1);
      Length_2 : constant Integer := X'Length (2);
   begin
      for I in 0 .. Length_1 - 1 loop
         for J in 0 .. Length_2 - 1 loop
            pragma Loop_Optimize (Vector);
            Apply (
               X (X'First (1) + I, X'First (2) + J),
               Param (Param'First (1) + I, Param'First (2) + J));
         end loop;
      end loop;
   end Apply_Matrix;

   --  matrix arithmetic operations

   function Operator_Matrix (Right : Matrix) return Result_Matrix is
   begin
      return Result : Result_Matrix (Right'Range (1), Right'Range (2)) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               pragma Loop_Optimize (Vector);
               Result (I, J) := Operator (Right (I, J));
            end loop;
         end loop;
      end return;
   end Operator_Matrix;

   function Operator_Matrix_Param (X : Matrix; Y : Parameter_Type)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (X'Range (1), X'Range (2)) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               pragma Loop_Optimize (Vector);
               Result (I, J) := Operator (X (I, J), Y);
            end loop;
         end loop;
      end return;
   end Operator_Matrix_Param;

   function Operator_Matrix_Matrix (Left : Left_Matrix; Right : Right_Matrix)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (Left'Range (1), Left'Range (2)) do
         declare
            Length_1 : constant Integer := Result'Length (1);
            Length_2 : constant Integer := Result'Length (2);
         begin
            for I in 0 .. Length_1 - 1 loop
               for J in 0 .. Length_2 - 1 loop
                  pragma Loop_Optimize (Vector);
                  Result (Result'First (1) + I, Result'First (2) + J) :=
                     Operator (
                        Left (Left'First (1) + I, Left'First (2) + J),
                        Right (Right'First (1) + I, Right'First (2) + J));
               end loop;
            end loop;
         end;
      end return;
   end Operator_Matrix_Matrix;

   function Operator_Matrix_Matrix_Param (X, Y : Matrix; Z : Parameter_Type)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (X'Range (1), X'Range (2)) do
         declare
            Length_1 : constant Integer := Result'Length (1);
            Length_2 : constant Integer := Result'Length (2);
         begin
            for I in 0 .. Length_1 - 1 loop
               for J in 0 .. Length_2 - 1 loop
                  pragma Loop_Optimize (Vector);
                  Result (Result'First (1) + I, Result'First (2) + J) :=
                     Operator (
                        X (X'First (1) + I, X'First (2) + J),
                        Y (Y'First (1) + I, Y'First (2) + J),
                        Z);
               end loop;
            end loop;
         end;
      end return;
   end Operator_Matrix_Matrix_Param;

   function Transpose (X : Matrix) return Matrix is
   begin
      return Result : Matrix (X'Range (2), X'Range (1)) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               pragma Loop_Optimize (Vector);
               Result (I, J) := X (J, I);
            end loop;
         end loop;
      end return;
   end Transpose;

   function Multiply_Matrix_Matrix (Left : Left_Matrix; Right : Right_Matrix)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (Left'Range (1), Right'Range (2)) do
         declare
            Length_Folded : constant Integer := Left'Length (2);
         begin
            for I in Result'Range (1) loop
               for K in 0 .. Length_Folded - 1 loop
                  for J in Result'Range (2) loop
                     pragma Loop_Optimize (Vector);
                     declare
                        Z : Result_Type;
                     begin
                        if K = 0 then
                           Z := Zero;
                        else
                           Z := Result (I, J);
                        end if;
                        Result (I, J) :=
                           Z
                           + Left (I, Left'First (2) + K)
                              * Right (Right'First (1) + K, J);
                     end;
                  end loop;
               end loop;
            end loop;
         end;
      end return;
   end Multiply_Matrix_Matrix;

   function Multiply_Vector_Vector (Left : Left_Vector; Right : Right_Vector)
      return Result_Matrix is
   begin
      return Result : Result_Matrix (Left'Range, Right'Range) do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               pragma Loop_Optimize (Vector);
               Result (I, J) := Left (I) * Right (J);
            end loop;
         end loop;
      end return;
   end Multiply_Vector_Vector;

   function Multiply_Vector_Matrix (Left : Left_Vector; Right : Right_Matrix)
      return Result_Vector is
   begin
      return Result : Result_Vector (Right'Range (2)) do
         declare
            Length_Folded : constant Integer := Left'Length;
         begin
            for J in Result'Range loop
               declare
                  Z : Result_Type := Zero;
               begin
                  for K in 0 .. Length_Folded - 1 loop
                     pragma Loop_Optimize (Vector);
                     Z := Z
                        + Left (Left'First + K)
                           * Right (Right'First (1) + K, J);
                  end loop;
                  Result (J) := Z;
               end;
            end loop;
         end;
      end return;
   end Multiply_Vector_Matrix;

   function Multiply_Matrix_Vector (Left : Left_Matrix; Right : Right_Vector)
      return Result_Vector is
   begin
      return Result : Result_Vector (Left'Range (1)) do
         declare
            Length_Folded : constant Integer := Right'Length;
         begin
            for I in Result'Range loop
               declare
                  Z : Result_Type := Zero;
               begin
                  for K in 0 .. Length_Folded - 1 loop
                     pragma Loop_Optimize (Vector);
                     Z := Z
                        + Left (I, Left'First (2) + K)
                           * Right (Right'First + K);
                  end loop;
                  Result (I) := Z;
               end;
            end loop;
         end;
      end return;
   end Multiply_Matrix_Vector;

   --  matrix inversion and related operations

   function Minor (A : Matrix; I, J : Integer) return Matrix is
   begin
      return Result : Matrix (
         A'First (1) + 1 .. A'Last (1),
         A'First (2) + 1 .. A'Last (2))
      do
         for I2 in Result'First (1) .. I loop
            for J2 in Result'First (2) .. J loop
               pragma Loop_Optimize (Vector);
               Result (I2, J2) := A (I2 - 1, J2 - 1);
            end loop;
            for J2 in J + 1 .. Result'Last (2) loop
               pragma Loop_Optimize (Vector);
               Result (I2, J2) := A (I2 - 1, J2);
            end loop;
         end loop;
         for I2 in I + 1 .. Result'Last (1) loop
            for J2 in Result'First (2) .. J loop
               pragma Loop_Optimize (Vector);
               Result (I2, J2) := A (I2, J2 - 1);
            end loop;
            for J2 in J + 1 .. Result'Last (2) loop
               pragma Loop_Optimize (Vector);
               Result (I2, J2) := A (I2, J2);
            end loop;
         end loop;
      end return;
   end Minor;

   function Inverse (A : Matrix) return Matrix is
      detA : constant Number := Determinant (A);
      Master_Sign : Number := One;
      Sign : Number;
   begin
      return Result : Matrix (A'Range (2), A'Range (1)) do
         declare
            Length : constant Integer := Result'Length (1); -- square matrix
         begin
            for I in 0 .. Length - 1 loop
               for J in 0 .. Length - 1 loop
                  pragma Loop_Optimize (Vector);
                  if J rem 2 = 0 then
                     Sign := Master_Sign;
                  else
                     Sign := -Master_Sign;
                  end if;
                  Result (Result'First (1) + I, Result'First (2) + J) :=
                     Sign
                     * Determinant (
                        Minor (A, Result'First (1) + J, Result'First (2) + I))
                     / detA;
               end loop;
               Master_Sign := -Master_Sign;
            end loop;
         end;
      end return;
   end Inverse;

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
               for X in A'Range (1) loop -- cannot vectorize
                  Result := Result
                     + Sign
                        * A (X, A'First (2))
                        * Determinant (Minor (A, X, A'First (2)));
                  Sign := -Sign;
               end loop;
               return Result;
            end;
      end case;
   end Determinant;

   --  eigenvalues and vectors of a hermitian matrix

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
            pragma Loop_Optimize (Vector);
            eigenvecs (i, j) := Zero;
         end loop;
      end loop;
      for i in 0 .. eigenvecs'Length (1) - 1 loop
         pragma Loop_Optimize (Vector);
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
                     pragma Loop_Optimize (Vector);
                     v1 := mat (mat'First (1) + k, mat'First (2) + i);
                     v2 := mat (mat'First (1) + k, mat'First (2) + j);
                     mat (mat'First (1) + k, mat'First (2) + i) :=
                        v1 * y11 + v2 * y12;
                     mat (mat'First (1) + k, mat'First (2) + j) :=
                        v1 * y21 + v2 * y22;
                  end loop;
                  for k in 0 .. n - 1 loop
                     pragma Loop_Optimize (Vector);
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
                     pragma Loop_Optimize (Vector);
                     v1 := eigenvecs (
                        eigenvecs'First (1) + i,
                        eigenvecs'First (2) + k);
                     v2 := eigenvecs (
                        eigenvecs'First (1) + j,
                        eigenvecs'First (2) + k);
                     eigenvecs (
                        eigenvecs'First (1) + i,
                        eigenvecs'First (2) + k) := v1 * y11 + v2 * y12;
                     eigenvecs (
                        eigenvecs'First (1) + j,
                        eigenvecs'First (2) + k) := v1 * y21 + v2 * y22;
                  end loop;
               end if;
            end loop;
         end loop;
      end loop;
      declare
         Length : constant Integer := lami'Length;
      begin
         for i in 0 .. Length - 1 loop
            pragma Loop_Optimize (Vector);
            lami (lami'First + i) :=
               To_Real (mat (mat'First (1) + i, mat'First (2) + i));
         end loop;
      end;
   end Eigensystem;

   --  other matrix operations

   function Unit_Matrix (Order : Positive; First_1, First_2 : Integer := 1)
      return Matrix is
   begin
      return Result : Matrix (
         First_1 .. First_1 + Order - 1,
         First_2 .. First_2 + Order - 1)
      do
         for I in Result'Range (1) loop
            for J in Result'Range (2) loop
               pragma Loop_Optimize (Vector);
               Result (I, J) := Zero;
            end loop;
         end loop;
         for I in 0 .. Order - 1 loop
            pragma Loop_Optimize (Vector);
            Result (First_1 + I, First_2 + I) := One;
         end loop;
      end return;
   end Unit_Matrix;

end Ada.Numerics.Generic_Arrays;
