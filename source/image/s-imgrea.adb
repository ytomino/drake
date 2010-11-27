with System.Fore;
with System.Formatting;
with System.Long_Long_Float_Divide;
package body System.Img_Real is
   pragma Suppress (All_Checks);

   function isnan (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, isnan, "__builtin_isnanl");

   function isinf (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, isinf, "__builtin_isinfl");

   function signbit (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, signbit, "__builtin_signbitl");

   procedure Split (
      X : Long_Long_Float;
      Fraction : out Long_Long_Float;
      Exponent : out Integer;
      Base : Formatting.Base_Type);
   procedure Split (
      X : Long_Long_Float;
      Fraction : out Long_Long_Float;
      Exponent : out Integer;
      Base : Formatting.Base_Type) is
   begin
      if X >= Long_Long_Float (Base) then
         declare
            B : Long_Long_Float := Long_Long_Float (Base);
         begin
            Exponent := 1;
            loop
               declare
                  Next_B : constant Long_Long_Float :=
                     B * Long_Long_Float (Base);
               begin
                  exit when Next_B > X;
                  B := Next_B;
               end;
               Exponent := Exponent + 1;
            end loop;
            Fraction := X / B;
         end;
      elsif X >= 0.0 then
         Fraction := X;
         Exponent := 0;
         while Fraction < 1.0 loop
            Fraction := Fraction * Long_Long_Float (Base);
            Exponent := Exponent - 1;
         end loop;
      else
         Fraction := X;
         Exponent := 0;
      end if;
   end Split;

   procedure Fill_Decimal (
      Value : Long_Long_Float;
      S : in out String;
      Position : in out Positive;
      Width : Positive);
   procedure Fill_Decimal (
      Value : Long_Long_Float;
      S : in out String;
      Position : in out Positive;
      Width : Positive)
   is
      X : Long_Long_Float := Value;
      Int_Part : Long_Long_Float;
   begin
      S (Position) := '.';
      Position := Position + 1;
      for I in 2 .. Width loop --  drop last one
         X := X * 10.0;
         Int_Part := Long_Long_Float'Truncation (X);
         X := X - Int_Part;
         S (Position) := Character'Val (Character'Pos ('0') +
            Integer (Int_Part));
         Position := Position + 1;
      end loop;
      X := X * 10.0;
      Int_Part := Long_Long_Float'Rounding (X); --  last one
      S (Position) := Character'Val (Character'Pos ('0') + Integer (Int_Part));
      Position := Position + 1;
   end Fill_Decimal;

   procedure Image_Ordinary_Fixed_Point (
      V : Long_Long_Float;
      S : in out String;
      P : out Natural;
      Aft : Natural)
   is
      Abs_V : Long_Long_Float;
      Int_Part : Long_Long_Float;
      Dec_Part : Long_Long_Float;
      Position : Positive := S'First;
      Next : Positive;
   begin
      if signbit (V) /= 0 then
         S (Position) := '-';
         Abs_V := -V;
      else
         S (Position) := ' ';
         Abs_V := V;
      end if;
      Position := Position + 1;
      Int_Part := Long_Long_Float'Truncation (Abs_V);
      Dec_Part := Abs_V - Int_Part;
      --  integer part
      Next := Position + Fore.Fore_Width (Int_Part);
      for I in reverse Position .. Next - 1 loop
         --  FreeBSD does not have "remquol"
         declare
            Q : Long_Long_Float;
            R : Long_Long_Float;
         begin
            Long_Long_Float_Divide (Int_Part, 10.0, Q, R);
            S (I) := Character'Val (Character'Pos ('0') + Integer (R));
            Int_Part := Q;
         end;
      end loop;
      Position := Next;
      --  '.' and decimal part
      Fill_Decimal (Dec_Part, S, Position, Width => Aft);
      P := Position - 1;
   end Image_Ordinary_Fixed_Point;

   procedure Image_Floating_Point (
      V : Long_Long_Float;
      S : in out String;
      P : out Natural;
      Digs : Natural)
   is
      Position : Positive := S'First;
      Abs_V : Long_Long_Float;
   begin
      if signbit (V) /= 0 then
         S (Position) := '-';
         Abs_V := -V;
      else
         S (Position) := ' ';
         Abs_V := V;
      end if;
      Position := Position + 1;
      if isnan (V) /= 0 then
         P := Position + 2;
         S (Position .. P) := "NAN";
      elsif isinf (V) /= 0 then
         P := Position + 2;
         S (Position .. P) := "INF";
      else
         declare
            Fraction : Long_Long_Float;
            Exponent : Integer;
            Int_Part : Long_Long_Float;
            Error : Boolean;
         begin
            Split (Abs_V, Fraction, Exponent, Base => 10);
            --  integer part
            Int_Part := Long_Long_Float'Truncation (Fraction);
            Fraction := Fraction - Int_Part;
            S (Position) := Character'Val (
               Character'Pos ('0') + Integer (Int_Part));
            Position := Position + 1;
            --  '.' and decimal part
            Fill_Decimal (Fraction, S, Position, Width => Digs - 1);
            --  exponent
            S (Position) := 'E';
            Position := Position + 1;
            if Exponent < 0 then
               S (Position) := '-';
               Exponent := -Exponent;
            else
               S (Position) := '+';
            end if;
            Position := Position + 1;
            Formatting.Image (
               Formatting.Unsigned (Exponent),
               S (Position .. S'Last),
               P,
               Width => 2,
               Padding => '0',
               Error => Error);
            pragma Assert (not Error);
         end;
      end if;
   end Image_Floating_Point;

end System.Img_Real;
