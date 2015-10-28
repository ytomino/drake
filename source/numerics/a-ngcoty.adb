with Ada.Float;
package body Ada.Numerics.Generic_Complex_Types is

   subtype Float is Standard.Float; -- hiding "Float" package

   function constant_p (x : Complex) return Integer
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_constant_p";
   pragma Warnings (Off, constant_p); -- [gcc-5] excessive prototype checking

   package Elementary_Functions is

      subtype Float_Type is Real;

      function Sin (X : Float_Type'Base) return Float_Type'Base;
      function Cos (X : Float_Type'Base) return Float_Type'Base;

   end Elementary_Functions;

   package body Elementary_Functions is

      function Sin (X : Float_Type'Base) return Float_Type'Base is
      begin
         if Float_Type'Digits <= Float'Digits then
            declare
               function sinf (A1 : Float) return Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_sinf";
            begin
               return Float_Type'Base (sinf (Float (X)));
            end;
         elsif Float_Type'Digits <= Long_Float'Digits then
            declare
               function sin (A1 : Long_Float) return Long_Float
                  with Import,
                     Convention => Intrinsic, External_Name => "__builtin_sin";
            begin
               return Float_Type'Base (sin (Long_Float (X)));
            end;
         else
            declare
               function sinl (x : Long_Long_Float) return Long_Long_Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_sinl";
            begin
               return Float_Type'Base (sinl (Long_Long_Float (X)));
            end;
         end if;
      end Sin;

      function Cos (X : Float_Type'Base) return Float_Type'Base is
      begin
         if Float_Type'Digits <= Float'Digits then
            declare
               function cosf (A1 : Float) return Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_cosf";
            begin
               return Float_Type'Base (cosf (Float (X)));
            end;
         elsif Float_Type'Digits <= Long_Float'Digits then
            declare
               function cos (A1 : Long_Float) return Long_Float
                  with Import,
                     Convention => Intrinsic, External_Name => "__builtin_cos";
            begin
               return Float_Type'Base (cos (Long_Float (X)));
            end;
         else
            declare
               function cosl (x : Long_Long_Float) return Long_Long_Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_cosl";
            begin
               return Float_Type'Base (cosl (Long_Long_Float (X)));
            end;
         end if;
      end Cos;

   end Elementary_Functions;

   --  implementation

   function i return Imaginary is
   begin
      return 1.0;
   end i;

   function Re (X : Complex) return Real'Base is
   begin
      return X.Re;
   end Re;

   function Im (X : Complex) return Real'Base is
   begin
      return X.Im;
   end Im;

   function Im (X : Imaginary) return Real'Base is
   begin
      return Real'Base (X);
   end Im;

   procedure Set_Re (X : in out Complex; Re : Real'Base) is
   begin
      X.Re := Re;
   end Set_Re;

   procedure Set_Im (X : in out Complex; Im : Real'Base) is
   begin
      X.Im := Im;
   end Set_Im;

   procedure Set_Im (X : out Imaginary; Im : Real'Base) is
   begin
      X := Imaginary (Im);
   end Set_Im;

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

   function Modulus (X : Complex) return Real'Base is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function cabsf (x : Complex) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_cabsf";
         begin
            return Real'Base (cabsf (X));
         end;
      elsif Real'Digits <= Long_Float'Digits
         and then Real'Size <= Long_Float'Size -- for 32bit FreeBSD
      then
         declare
            function cabs (x : Complex) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_cabs";
         begin
            return Real'Base (cabs (X));
         end;
      else
         declare
            function cabsl (x : Complex) return Long_Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_cabsl";
         begin
            return Real'Base (cabsl (X));
         end;
      end if;
   end Modulus;

   function Argument (X : Complex) return Real'Base is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function cargf (x : Complex) return Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_cargf";
         begin
            return Real'Base (cargf (X));
         end;
      elsif Real'Digits <= Long_Float'Digits
         and then Real'Size <= Long_Float'Size -- for 32bit FreeBSD
      then
         declare
            function carg (x : Complex) return Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_carg";
         begin
            return Real'Base (carg (X));
         end;
      else
         declare
            function cargl (x : Complex) return Long_Long_Float
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_cargl";
         begin
            return Real'Base (cargl (X));
         end;
      end if;
   end Argument;

   function Argument (X : Complex; Cycle : Real'Base) return Real'Base is
   begin
      if not Standard'Fast_Math and then not (Cycle > 0.0) then
         raise Argument_Error; -- CXG2006
      end if;
      return Argument (X) * Cycle / (2.0 * Real'Base'(Pi));
   end Argument;

   function Compose_From_Polar (Modulus, Argument : Real'Base)
      return Complex is
   begin
      return (
         Re => Modulus * Elementary_Functions.Cos (Argument),
         Im => Modulus * Elementary_Functions.Sin (Argument));
   end Compose_From_Polar;

   function Compose_From_Polar (Modulus, Argument, Cycle : Real'Base)
      return Complex
   is
      R : Real'Base;
   begin
      if not Standard'Fast_Math then
         if not (Cycle > 0.0) then
            raise Argument_Error; -- CXG2007
         end if;
         declare
            procedure Modulo_Divide_By_1 is
               new Ada.Float.Modulo_Divide_By_1 (
                  Real'Base,
                  Real'Base,
                  Real'Base);
            Q : Real'Base;
         begin
            Modulo_Divide_By_1 (Argument / Cycle, Q, R);
            if R = 0.25 then
               return (Re => 0.0, Im => Modulus);
            elsif R = 0.5 then
               return (Re => -Modulus, Im => 0.0);
            elsif R = 0.75 then
               return (Re => 0.0, Im => -Modulus);
            end if;
         end;
      else
         R := Argument / Cycle;
      end if;
      return Compose_From_Polar (Modulus, 2.0 * Real'Base'(Pi) * R);
   end Compose_From_Polar;

   function "+" (Right : Complex) return Complex is
   begin
      return Right;
   end "+";

   function "-" (Right : Complex) return Complex is
   begin
      return (Re => -Right.Re, Im => -Right.Im);
   end "-";

   function Conjugate (X : Complex) return Complex is
   begin
      if Real'Digits <= Float'Digits then
         declare
            function conjf (x : Complex) return Complex
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_conjf";
         begin
            return conjf (X);
         end;
      elsif Real'Digits <= Long_Float'Digits
         and then Real'Size <= Long_Float'Size -- for 32bit FreeBSD
      then
         declare
            function conj (x : Complex) return Complex
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_conj";
         begin
            return conj (X);
         end;
      else
         declare
            function conjl (x : Complex) return Complex
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_conjl";
         begin
            return conjl (X);
         end;
      end if;
   end Conjugate;

   function "+" (Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re + Right.Re, Im => Left.Im + Right.Im);
   end "+";

   function "-" (Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re - Right.Re, Im => Left.Im - Right.Im);
   end "-";

   function "*" (Left, Right : Complex) return Complex is
      Result : Complex;
   begin
      if Standard'Fast_Math
         or else (constant_p (Left) /= 0 and then constant_p (Right) /= 0)
      then
         Result.Re := Left.Re * Right.Re - Left.Im * Right.Im;
         Result.Im := Left.Re * Right.Im + Left.Im * Right.Re;
      else
         --  use libgcc
         if Real'Digits <= Float'Digits then
            declare
               function mulsc3 (
                  Left_Re, Left_Im, Right_Re, Right_Im : Float)
                  return Complex
                  with Import, Convention => C, External_Name => "__mulsc3";
               pragma Pure_Function (mulsc3);
            begin
               Result := mulsc3 (
                  Float (Left.Re),
                  Float (Left.Im),
                  Float (Right.Re),
                  Float (Right.Im));
            end;
         elsif Real'Digits <= Long_Float'Digits
            and then Real'Size <= Long_Float'Size -- for 32bit FreeBSD
         then
            declare
               function muldc3 (
                  Left_Re, Left_Im, Right_Re, Right_Im : Long_Float)
                  return Complex
                  with Import, Convention => C, External_Name => "__muldc3";
               pragma Pure_Function (muldc3);
            begin
               Result := muldc3 (
                  Long_Float (Left.Re),
                  Long_Float (Left.Im),
                  Long_Float (Right.Re),
                  Long_Float (Right.Im));
            end;
         else
            declare
               function mulxc3 (
                  Left_Re, Left_Im, Right_Re, Right_Im : Long_Long_Float)
                  return Complex
                  with Import, Convention => C, External_Name => "__mulxc3";
               pragma Pure_Function (mulxc3);
            begin
               Result := mulxc3 (
                  Long_Long_Float (Left.Re),
                  Long_Long_Float (Left.Im),
                  Long_Long_Float (Right.Re),
                  Long_Long_Float (Right.Im));
            end;
         end if;
      end if;
      if not Standard'Fast_Math then
         --  CXG2020
         declare
            function Is_Infinity is new Ada.Float.Is_Infinity (Real'Base);
         begin
            if Is_Infinity (Result.Re) then
               declare
                  Re_2 : constant Real'Base :=
                     4.0 * (
                        Real'Base'(Left.Re / 2.0)
                           * Real'Base'(Right.Re / 2.0)
                        - Real'Base'(Left.Im / 2.0)
                           * Real'Base'(Right.Im / 2.0));
               begin
                  if not Is_Infinity (Re_2) then -- keep a sign of INF or NaN
                     Result.Re := Re_2;
                  end if;
               end;
            end if;
            if Is_Infinity (Result.Im) then
               declare
                  Im_2 : constant Real'Base :=
                     4.0 * (
                        Real'Base'(Left.Re / 2.0)
                           * Real'Base'(Right.Im / 2.0)
                        + Real'Base'(Left.Im / 2.0)
                           * Real'Base'(Right.Re / 2.0));
               begin
                  if not Is_Infinity (Im_2) then
                     Result.Im := Im_2;
                  end if;
               end;
            end if;
         end;
      end if;
      return Result;
   end "*";

   function "/" (Left, Right : Complex) return Complex is
      Result : Complex;
   begin
      if Standard'Fast_Math
         or else (constant_p (Left) /= 0 and then constant_p (Right) /= 0)
      then
         Result.Re :=
            (Left.Re * Right.Re + Left.Im * Right.Im)
            / (Right.Re * Right.Re + Right.Im * Right.Im);
         Result.Im :=
            (Left.Im * Right.Re - Left.Re * Right.Im)
            / (Right.Re * Right.Re + Right.Im * Right.Im);
      else
         --  use libgcc
         if Real'Digits <= Float'Digits then
            declare
               function divsc3 (
                  Left_Re, Left_Im, Right_Re, Right_Im : Float)
                  return Complex
                  with Import, Convention => C, External_Name => "__divsc3";
               pragma Pure_Function (divsc3);
            begin
               Result := divsc3 (
                  Float (Left.Re),
                  Float (Left.Im),
                  Float (Right.Re),
                  Float (Right.Im));
            end;
         elsif Real'Digits <= Long_Float'Digits
            and then Real'Size <= Long_Float'Size -- for 32bit FreeBSD
         then
            declare
               function divdc3 (
                  Left_Re, Left_Im, Right_Re, Right_Im : Long_Float)
                  return Complex
                  with Import, Convention => C, External_Name => "__divdc3";
               pragma Pure_Function (divdc3);
            begin
               Result := divdc3 (
                  Long_Float (Left.Re),
                  Long_Float (Left.Im),
                  Long_Float (Right.Re),
                  Long_Float (Right.Im));
            end;
         else
            declare
               function divxc3 (
                  Left_Re, Left_Im, Right_Re, Right_Im : Long_Long_Float)
                  return Complex
                  with Import, Convention => C, External_Name => "__divxc3";
               pragma Pure_Function (divxc3);
            begin
               Result := divxc3 (
                  Long_Long_Float (Left.Re),
                  Long_Long_Float (Left.Im),
                  Long_Long_Float (Right.Re),
                  Long_Long_Float (Right.Im));
            end;
         end if;
      end if;
      return Result;
   end "/";

   function "**" (Left : Complex; Right : Integer) return Complex is
   begin
      return Compose_From_Polar (
         Modulus (Left) ** Right,
         Argument (Left) * Real'Base (Right));
   end "**";

   function "abs" (Right : Imaginary) return Real'Base is
   begin
      return abs Real'Base (Right);
   end "abs";

   function "*" (Left, Right : Imaginary) return Real'Base is
   begin
      return -(Real'Base (Left) * Real'Base (Right));
   end "*";

   function "/" (Left, Right : Imaginary) return Real'Base is
   begin
      return Real'Base (Left) / Real'Base (Right);
   end "/";

   function "**" (Left : Imaginary; Right : Integer) return Complex is
   begin
      return Compose_From_Polar (
         (abs Real'Base (Left)) ** Right,
         (Real'Base'(Pi) / Real'Base'(2.0)) * Real'Base (Right));
   end "**";

   function "+" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re + Right, Im => Left.Im);
   end "+";

   function "+" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left + Right.Re, Im => Right.Im);
   end "+";

   function "-" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re - Right, Im => Left.Im);
   end "-";

   function "-" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left - Right.Re, Im => -Right.Im);
   end "-";

   function "*" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re * Right, Im => Left.Im * Right);
   end "*";

   function "*" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left * Right.Re, Im => Left * Right.Im);
   end "*";

   function "/" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re / Right, Im => Left.Im / Right);
   end "/";

   function "/" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (
         Re => (Left * Right.Re)
            / (Right.Re * Right.Re + Right.Im * Right.Im),
         Im => -(Left * Right.Im)
            / (Right.Re * Right.Re + Right.Im * Right.Im));
   end "/";

   function "+" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (Re => Left.Re, Im => Left.Im + Real'Base (Right));
   end "+";

   function "+" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (Re => Right.Re, Im => Real'Base (Left) + Right.Im);
   end "+";

   function "-" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (Re => Left.Re, Im => Left.Im - Real'Base (Right));
   end "-";

   function "-" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (Re => -Right.Re, Im => Real'Base (Left) - Right.Im);
   end "-";

   function "*" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (
         Re => -(Left.Im * Real'Base (Right)),
         Im => Left.Re * Real'Base (Right));
   end "*";

   function "*" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (
         Re => -(Real'Base (Left) * Right.Im),
         Im => Real'Base (Left) * Right.Re);
   end "*";

   function "/" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (
         Re => Left.Im / Real'Base (Right),
         Im => -(Left.Re / Real'Base (Right)));
   end "/";

   function "/" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (
         Re => (Real'Base (Left) * Right.Im)
            / (Right.Re * Right.Re + Right.Im * Right.Im),
         Im => (Real'Base (Left) * Right.Re)
            / (Right.Re * Right.Re + Right.Im * Right.Im));
   end "/";

   function "+" (Left : Imaginary; Right : Real'Base) return Complex is
   begin
      return (Re => Right, Im => Real'Base (Left));
   end "+";

   function "+" (Left : Real'Base; Right : Imaginary) return Complex is
   begin
      return (Re => Left, Im => Real'Base (Right));
   end "+";

   function "-" (Left : Imaginary; Right : Real'Base) return Complex is
   begin
      return (Re => -Right, Im => Real'Base (Left));
   end "-";

   function "-" (Left : Real'Base; Right : Imaginary) return Complex is
   begin
      return (Re => Left, Im => -Real'Base (Right));
   end "-";

   function "*" (Left : Imaginary; Right : Real'Base) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) * Right);
   end "*";

   function "*" (Left : Real'Base; Right : Imaginary) return Imaginary is
   begin
      return Imaginary (Left * Real'Base (Right));
   end "*";

   function "/" (Left : Imaginary; Right : Real'Base) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) / Right);
   end "/";

   function "/" (Left : Real'Base; Right : Imaginary) return Imaginary is
   begin
      return Imaginary (-(Left / Real'Base (Right)));
   end "/";

end Ada.Numerics.Generic_Complex_Types;
