with Ada.Numerics.Generic_Elementary_Functions;
package body Ada.Numerics.Generic_Complex_Elementary_Functions is
   pragma Suppress (All_Checks);

   package Real_Functions is new Generic_Elementary_Functions (Real'Base);
   --  Arctan, Cos, Cosh, Exp, Log, Sin, Sinh, Sqrt

   Sqrt_2 : constant := 1.4142135623730950488016887242096980785696;
   Log_2 : constant := 0.6931471805599453094172321214581765680755;
   Pi_Div_2 : constant := Pi / 2.0;
   Square_Root_Epsilon : constant Real'Base
      := Sqrt_2 ** (1 - Real'Base'Model_Mantissa);
   Inv_Square_Root_Epsilon : constant Real'Base :=
      Sqrt_2 ** (Real'Base'Model_Mantissa - 1);
   Root_Root_Epsilon : constant Real'Base :=
      Sqrt_2 ** ((1 - Real'Base'Model_Mantissa) / 2);
   Log_Inverse_Epsilon_2 : constant Real'Base :=
      Real'Base (Real'Base'Model_Mantissa - 1) / 2.0;

   function Arccos (X : Complex) return Complex is
      Result : Complex;
   begin
      if X.Re = 1.0 and then X.Im = 0.0 then
         return (Re => 0.0, Im => 0.0);
      elsif abs Re (X) < Square_Root_Epsilon
         and then abs Im (X) < Square_Root_Epsilon
      then
         return (Re => Pi_Div_2, Im => 0.0) - X;
      elsif abs Re (X) > Inv_Square_Root_Epsilon
         or else abs Im (X) > Inv_Square_Root_Epsilon
      then
         return -2.0 * i * Log (
            Sqrt ((1.0 + X) / 2.0) + i * Sqrt ((1.0 - X) / 2.0));
      end if;
      Result := -i * Log (X + i * Sqrt (1.0 - X * X));
      if Im (X) = 0.0
        and then abs Re (X) <= 1.00
      then
         Set_Im (Result, Im (X));
      end if;
      return Result;
   end Arccos;

   function Arccosh (X : Complex) return Complex is
      Result : Complex;
   begin
      if X.Re = 1.0 and then X.Im = 0.0 then
         return (Re => 0.0, Im => 0.0);
      elsif abs Re (X) < Square_Root_Epsilon
         and then abs Im (X) < Square_Root_Epsilon
      then
         Result := Compose_From_Cartesian (-Im (X), -Pi_Div_2 + Re (X));
      elsif abs Re (X) > Inv_Square_Root_Epsilon
         or else abs Im (X) > Inv_Square_Root_Epsilon
      then
         Result := Log_2 + Log (X);
      else
         Result := 2.0 * Log (Sqrt ((1.0 + X) / 2.0) + Sqrt ((X - 1.0) / 2.0));
      end if;
      if Re (Result) <= 0.0 then
         Result := -Result;
      end if;
      return Result;
   end Arccosh;

   function Arccot (X : Complex) return Complex is
   begin
      return Pi / 2.0 - Arctan (X);
   end Arccot;

   function Arccoth (X : Complex) return Complex is
   begin
      if X.Re = 0.0 and then X.Im = 0.0 then
         return (0.0, Pi / 2.0);
      else
         return Arctanh (1.0 / X);
      end if;
   end Arccoth;

   function Arcsin (X : Complex) return Complex is
      Result : Complex;
   begin
      if abs Re (X) < Square_Root_Epsilon
         and then abs Im (X) < Square_Root_Epsilon
      then
         return X;
      elsif abs Re (X) > Inv_Square_Root_Epsilon
         or else abs Im (X) > Inv_Square_Root_Epsilon
      then
         Result := -i * (Log (i * X) + Log (Compose_From_Cartesian (2.0 * i)));
         if Im (Result) > Pi_Div_2 then
            Set_Im (Result, Pi - Im (X));
         elsif Im (Result) < -Pi_Div_2 then
            Set_Im (Result, -(Pi + Im (X)));
         end if;
         return Result;
      end if;
      Result := -i * Log (i * X + Sqrt (1.0 - X * X));
      if Re (X) = 0.0 then
         Set_Re (Result, Re (X));
      elsif Im (X) = 0.0
        and then abs Re (X) <= 1.00
      then
         Set_Im (Result, Im (X));
      end if;
      return Result;
   end Arcsin;

   function Arcsinh (X : Complex) return Complex is
      Result : Complex;
   begin
      if abs Re (X) < Square_Root_Epsilon
         and then abs Im (X) < Square_Root_Epsilon
      then
         return X;
      elsif abs Re (X) > Inv_Square_Root_Epsilon
         or else abs Im (X) > Inv_Square_Root_Epsilon
      then
         Result := Log_2 + Log (X); -- may have wrong sign
         if (Re (X) < 0.0 and then Re (Result) > 0.0)
           or else (Re (X) > 0.0 and then Re (Result) < 0.0)
         then
            Set_Re (Result, -Re (Result));
         end if;
         return Result;
      end if;
      Result := Log (X + Sqrt (1.0 + X * X));
      if Re (X) = 0.0 then
         Set_Re (Result, Re (X));
      elsif Im  (X) = 0.0 then
         Set_Im (Result, Im  (X));
      end if;
      return Result;
   end Arcsinh;

   function Arctan (X : Complex) return Complex is
   begin
      if abs X.Re < Square_Root_Epsilon
         and then abs X.Im < Square_Root_Epsilon
      then
         return X;
      else
         declare
            IX : constant Complex := (Re => -X.Im, Im => X.Re);
            Y : constant Complex := -(Log (1.0 + IX) - Log (1.0 - IX)) / 2.0;
         begin
            return (Re => -Y.Im, Im => Y.Re);
         end;
      end if;
   end Arctan;

   function Arctanh (X : Complex) return Complex is
   begin
      if abs X.Re < Square_Root_Epsilon
         and then abs X.Im < Square_Root_Epsilon
      then
         return X;
      else
         return (Log (1.0 + X) - Log (1.0 - X)) / 2.0;
      end if;
   end Arctanh;

   function Cos (X : Complex) return Complex is
   begin
      return (
         Re => Real_Functions.Cos (X.Re)  * Real_Functions.Cosh (X.Im),
         Im => -(Real_Functions.Sin (X.Re) * Real_Functions.Sinh (X.Im)));
   end Cos;

   function Cosh (X : Complex) return Complex is
   begin
      return (
         Re => Real_Functions.Cosh (X.Re) * Real_Functions.Cos (X.Im),
         Im => Real_Functions.Sinh (X.Re) * Real_Functions.Sin (X.Im));
   end Cosh;

   function Cot (X : Complex) return Complex is
   begin
      return Cos (X) / Sin (X);
   end Cot;

   function Coth (X : Complex) return Complex is
   begin
      return Cosh (X) / Sinh (X);
   end Coth;

   function Exp (X : Complex) return Complex is
      Y : constant Real'Base := Real_Functions.Exp (X.Re);
   begin
      return (
         Re => Y * Real_Functions.Cos (X.Im),
         Im => Y * Real_Functions.Sin (X.Im));
   end Exp;

   function Exp (X : Imaginary) return Complex is
   begin
      return (
         Re => Real_Functions.Cos (Im (X)),
         Im => Real_Functions.Sin (Im (X)));
   end Exp;

   function Log (X : Complex) return Complex is
   begin
      if abs (1.0 - X.Re) < Root_Root_Epsilon
         and then abs X.Im < Root_Root_Epsilon
      then
         declare
            Z : constant Complex := (Re => X.Re - 1.0, Im => X.Im);
         begin
            return
               (1.0 - (1.0 / 2.0 - (1.0 / 3.0 - (1.0 / 4.0) * Z) * Z) * Z) * Z;
         end;
      else
         declare
            Result_Re : constant Real'Base := Real_Functions.Log (Modulus (X));
            Result_Im : Real'Base := Real_Functions.Arctan (X.Im, X.Re);
         begin
            if Result_Im > Pi then
               Result_Im := Result_Im - 2.0 * Pi;
            end if;
            return (Re => Result_Re, Im => Result_Im);
         end;
      end if;
   end Log;

   function Sin (X : Complex) return Complex is
   begin
      if abs X.Re < Square_Root_Epsilon
         and then abs X.Im < Square_Root_Epsilon
      then
         return X;
      else
         return (
            Re => Real_Functions.Sin (X.Re) * Real_Functions.Cosh (X.Im),
            Im => Real_Functions.Cos (X.Re) * Real_Functions.Sinh (X.Im));
      end if;
   end Sin;

   function Sinh (X : Complex) return Complex is
   begin
      if abs X.Re < Square_Root_Epsilon
         and then abs X.Re < Square_Root_Epsilon
      then
         return X;
      else
         return (
            Re => Real_Functions.Sinh (X.Re) * Real_Functions.Cos (X.Im),
            Im => Real_Functions.Cosh (X.Re) * Real_Functions.Sin (X.Im));
      end if;
   end Sinh;

   function Sqrt (X : Complex) return Complex is
   begin
      if X.Re = 0.0 and then X.Im = 0.0 then
         return X;
      elsif X.Im = 0.0 then
         if X.Re > 0.0 then
            return (Re => Real_Functions.Sqrt (X.Re), Im => 0.0);
         else
            return (Re => 0.0, Im => Real_Functions.Sqrt (-X.Re));
         end if;
      elsif X.Re = 0.0 then
         declare
            Y : constant Real'Base := Real_Functions.Sqrt (abs X.Im);
         begin
            if X.Im > 0.0 then
               return (Re => Y, Im => Y);
            else
               return (Re => Y, Im => -Y);
            end if;
         end;
      else
         declare
            A : constant Real'Base := Real_Functions.Sqrt (
               0.5 * (
                  Real_Functions.Sqrt (X.Re * X.Re + X.Im * X.Im) + abs X.Re));
            B : constant Real'Base := X.Im / (2.0 * A);
         begin
            if X.Re > 0.0 then
               if X.Im > 0.0 then
                  return (Re => A, Im => B);
               else
                  return (Re => A, Im => -B);
               end if;
            else
               if X.Im > 0.0 then
                  return (Re => B, Im => A);
               else
                  return (Re => B, Im => -A);
               end if;
            end if;
         end;
      end if;
   end Sqrt;

   function Tan (X : Complex) return Complex is
   begin
      if abs X.Re < Square_Root_Epsilon
         and then abs X.Im < Square_Root_Epsilon
      then
         return X;
      elsif X.Im > Log_Inverse_Epsilon_2 then
         return (Re => 0.0, Im => 1.0);
      elsif X.Im < -Log_Inverse_Epsilon_2 then
         return (Re => 0.0, Im => -1.0);
      else
         return Sin (X) / Cos (X);
      end if;
   end Tan;

   function Tanh (X : Complex) return Complex is
   begin
      if abs X.Re < Square_Root_Epsilon
         and then abs X.Im < Square_Root_Epsilon
      then
         return X;
      elsif X.Re > Log_Inverse_Epsilon_2 then
         return (Re => 1.0, Im => 0.0);
      elsif X.Re < -Log_Inverse_Epsilon_2 then
         return (Re => -1.0, Im => 0.0);
      else
         return Sinh (X) / Cosh (X);
      end if;
   end Tanh;

   function "**" (Left : Complex; Right : Complex) return Complex is
   begin
      if not Standard'Fast_Math
         and then Left.Re = 0.0 and then Left.Im = 0.0
         and then Right.Re = 0.0 and then Right.Im = 0.0
      then
         raise Argument_Error; -- CXG1004
      elsif Right.Re = 1.0 and Right.Im = 0.0 then
         return Left; -- CXG1005
      elsif Left.Re = 0.0 and Left.Im = 0.0 then
         return Left;
      elsif Left.Re = 1.0 and Left.Im = 0.0 then
         return Left;
      elsif Right.Re = 0.0 and Right.Im = 0.0 then
         return (Re => 1.0, Im => 0.0);
      else
         return Exp (Right * Log (Left));
      end if;
   end "**";

   function "**" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return Left ** (Re => Right, Im => 0.0);
   end "**";

   function "**" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left, Im => 0.0) ** Right;
   end "**";

end Ada.Numerics.Generic_Complex_Elementary_Functions;
