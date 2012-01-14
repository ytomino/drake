with Ada.Float.Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types.Inside;
package body Ada.Numerics.Generic_Complex_Types is
   pragma Suppress (All_Checks);

   package Impl is new Inside;
   function Is_Infinity is new Float.Is_Infinity (Real'Base);
   function Sin is new Float.Elementary_Functions.Sin (Real'Base);
   function Cos is new Float.Elementary_Functions.Cos (Real'Base);
   subtype Float is Standard.Float; -- hiding "Float" package

   --  implementation

   function Argument (X : Complex) return Real'Base
      renames Impl.Argument;

   function Argument (X : Complex; Cycle : Real'Base) return Real'Base is
   begin
      if not Standard'Fast_Math and then Cycle <= 0.0 then
         raise Argument_Error; -- CXG2006
      else
         return Argument (X) * Cycle / (Real'Base'(2.0) * Real'Base'(Pi));
      end if;
   end Argument;

   function Compose_From_Cartesian (Re, Im : Real'Base) return Complex is
   begin
      return (Re => Re, Im => Im);
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Re : Real'Base) return Complex is
   begin
      return (Re => Re, Im => 0.0);
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Im : Imaginary) return Complex is
   begin
      return (Re => 0.0, Im => Real'Base (Im));
   end Compose_From_Cartesian;

   function Compose_From_Polar (Modulus, Argument : Real'Base)
      return Complex is
   begin
      return (
         Re => Modulus * Cos (Argument),
         Im => Modulus * Sin (Argument));
   end Compose_From_Polar;

   function Compose_From_Polar (Modulus, Argument, Cycle : Real'Base)
      return Complex is
   begin
      if Standard'Fast_Math then
         return Compose_From_Polar (
            Modulus,
            Real'Base'(2.0) * Real'Base'(Pi) * Argument / Cycle);
      else
         if Cycle <= 0.0 then
            raise Argument_Error; -- CXG2007
         else
            declare
               R : constant Real'Base :=
                  Real'Base'Remainder (Argument / Cycle, 1.0);
            begin
               if R = 0.25 then
                  return (Re => 0.0, Im => Modulus);
               elsif R = 0.5 then
                  return (Re => -Modulus, Im => 0.0);
               elsif R = 0.75 then
                  return (Re => 0.0, Im => -Modulus);
               else
                  return Compose_From_Polar (
                     Modulus,
                     Real'Base'(2.0) * Real'Base'(Pi) * R);
               end if;
            end;
         end if;
      end if;
   end Compose_From_Polar;

   function Conjugate (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function conjf (A1 : Complex) return Complex;
            pragma Import (Intrinsic, conjf, "__builtin_conjf");
         begin
            return conjf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits then
         declare
            function conj (A1 : Complex) return Complex;
            pragma Import (Intrinsic, conj, "__builtin_conj");
         begin
            return conj (X);
         end;
      else
         declare
            function conjl (A1 : Complex) return Complex;
            pragma Import (Intrinsic, conjl, "__builtin_conjl");
         begin
            return conjl (X);
         end;
      end if;
   end Conjugate;

   function i return Imaginary is
   begin
      return 1.0;
   end i;

   function Im (X : Complex) return Real'Base is
   begin
      return X.Im;
   end Im;

   function Im (X : Imaginary) return Real'Base is
   begin
      return Real'Base (X);
   end Im;

   function Modulus (X : Complex) return Real'Base
      renames Impl.Modulus;

   function Re (X : Complex) return Real'Base is
   begin
      return X.Re;
   end Re;

   procedure Set_Im (X : in out Complex; Im : Real'Base) is
   begin
      X.Im := Im;
   end Set_Im;

   procedure Set_Im (X : out Imaginary; Im : Real'Base) is
   begin
      X := Imaginary (Im);
   end Set_Im;

   procedure Set_Re (X : in out Complex; Re : Real'Base) is
   begin
      X.Re := Re;
   end Set_Re;

   function "abs" (Right : Imaginary) return Real'Base is
   begin
      return abs Real'Base (Right);
   end "abs";

   function "+" (Right : Complex) return Complex is
   begin
      return Right;
   end "+";

   function "+" (Right : Imaginary) return Imaginary is
   begin
      return Right;
   end "+";

   function "+" (Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re + Right.Re, Im => Left.Im + Right.Im);
   end "+";

   function "+" (Left, Right : Imaginary) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) + Real'Base (Right));
   end "+";

   function "+" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re + Right, Im => Left.Im);
   end "+";

   function "+" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left + Right.Re, Im => Right.Im);
   end "+";

   function "+" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (Re => Left.Re, Im => Left.Im + Real'Base (Right));
   end "+";

   function "+" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (Re => Right.Re, Im => Real'Base (Left) + Right.Im);
   end "+";

   function "+" (Left : Imaginary; Right : Real'Base) return Complex is
   begin
      return (Re => Right, Im => Real'Base (Left));
   end "+";

   function "+" (Left : Real'Base; Right : Imaginary) return Complex is
   begin
      return (Re => Left, Im => Real'Base (Right));
   end "+";

   function "-" (Right : Complex) return Complex is
   begin
      return (Re => -Right.Re, Im => -Right.Im);
   end "-";

   function "-" (Right : Imaginary) return Imaginary is
   begin
      return Imaginary (-Real'Base (Right));
   end "-";

   function "-" (Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re - Right.Re, Im => Left.Im - Right.Im);
   end "-";

   function "-" (Left, Right : Imaginary) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) - Real'Base (Right));
   end "-";

   function "-" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re - Right, Im => Left.Im);
   end "-";

   function "-" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left - Right.Re, Im => -Right.Im);
   end "-";

   function "-" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (Re => Left.Re, Im => Left.Im - Real'Base (Right));
   end "-";

   function "-" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (Re => -Right.Re, Im => Real'Base (Left) - Right.Im);
   end "-";

   function "-" (Left : Imaginary; Right : Real'Base) return Complex is
   begin
      return (Re => -Right, Im => Real'Base (Left));
   end "-";

   function "-" (Left : Real'Base; Right : Imaginary) return Complex is
   begin
      return (Re => Left, Im => -Real'Base (Right));
   end "-";

   function "*" (Left, Right : Complex) return Complex is
      Re : Real'Base := Left.Re * Right.Re - Left.Im * Right.Im;
      Im : Real'Base := Left.Re * Right.Im + Left.Im * Right.Re;
   begin
      if not Standard'Fast_Math then
         --  CXG2020
         if Is_Infinity (Re) then
            Re := 4.0 * (
               Real'Base'(Left.Re / 2.0) * Real'Base'(Right.Re / 2.0) -
               Real'Base'(Left.Im / 2.0) * Real'Base'(Right.Im / 2.0));
         end if;
         if Is_Infinity (Im) then
            Im := 4.0 * (
               Real'Base'(Left.Re / 2.0) * Real'Base'(Right.Im / 2.0) +
               Real'Base'(Left.Im / 2.0) * Real'Base'(Right.Re / 2.0));
         end if;
      end if;
      return (Re, Im);
   end "*";

   function "*" (Left, Right : Imaginary) return Real'Base is
   begin
      return -(Real'Base (Left) * Real'Base (Right));
   end "*";

   function "*" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re * Right, Im => Left.Im * Right);
   end "*";

   function "*" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left * Right.Re, Im => Left * Right.Im);
   end "*";

   function "*" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (Re => -(Left.Im * Real'Base (Right)),
              Im => Left.Re * Real'Base (Right));
   end "*";

   function "*" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (Re => -(Real'Base (Left) * Right.Im),
              Im => Real'Base (Left) * Right.Re);
   end "*";

   function "*" (Left : Imaginary; Right : Real'Base) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) * Right);
   end "*";

   function "*" (Left : Real'Base; Right : Imaginary) return Imaginary is
   begin
      return Imaginary (Left * Real'Base (Right));
   end "*";

   function "/" (Left, Right : Complex) return Complex is
   begin
      return (Re => (Left.Re * Right.Re + Left.Im * Right.Im) /
                    (Right.Re * Right.Re + Right.Im * Right.Im),
              Im => (Left.Im * Right.Re - Left.Re * Right.Im) /
                    (Right.Re * Right.Re + Right.Im * Right.Im));
   end "/";

   function "/" (Left, Right : Imaginary) return Real'Base is
   begin
      return Real'Base (Left) / Real'Base (Right);
   end "/";

   function "/" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re / Right, Im => Left.Im / Right);
   end "/";

   function "/" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => (Left * Right.Re) /
                    (Right.Re * Right.Re + Right.Im * Right.Im),
              Im => -(Left * Right.Im) /
                    (Right.Re * Right.Re + Right.Im * Right.Im));
   end "/";

   function "/" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (Re => Left.Im / Real'Base (Right),
              Im => -(Left.Re / Real'Base (Right)));
   end "/";

   function "/" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (Re => (Real'Base (Left) * Right.Im) /
                    (Right.Re * Right.Re + Right.Im * Right.Im),
              Im => (Real'Base (Left) * Right.Re) /
                    (Right.Re * Right.Re + Right.Im * Right.Im));
   end "/";

   function "/" (Left : Imaginary; Right : Real'Base) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) / Right);
   end "/";

   function "/" (Left : Real'Base; Right : Imaginary) return Imaginary is
   begin
      return Imaginary (-(Left / Real'Base (Right)));
   end "/";

   function "**" (Left : Complex; Right : Integer) return Complex is
   begin
      return Compose_From_Polar (
         Modulus (Left) ** Right,
         Argument (Left) * Real'Base (Right));
   end "**";

   function "**" (Left : Imaginary; Right : Integer) return Complex is
   begin
      return Compose_From_Polar (
         (abs Real'Base (Left)) ** Right,
         (Real'Base'(Pi) / Real'Base'(2.0)) * Real'Base (Right));
   end "**";

   function "<" (Left, Right : Imaginary) return Boolean is
   begin
      return Real'Base (Left) < Real'Base (Right);
   end "<";

   function "<=" (Left, Right : Imaginary) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function ">" (Left, Right : Imaginary) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">=" (Left, Right : Imaginary) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

end Ada.Numerics.Generic_Complex_Types;
