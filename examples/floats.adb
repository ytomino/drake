with Ada.Float;
-- FreeBSD 7.x does not have nan, nanf, nal, but 8.x have.
procedure floats is
	generic
		type T is digits <>;
	procedure Test;
	procedure Test is
		function Infinity is new Ada.Float.Infinity (T);
		function NaN is new Ada.Float.NaN (T);
		function Is_Infinity is new Ada.Float.Is_Infinity (T);
		function Is_NaN is new Ada.Float.Is_NaN (T);
		function Is_Negative is new Ada.Float.Is_Negative (T);
		X : T := T'Value ("2.0");
		type Unaligned is record
			Padding : Character;
			Value : T;
		end record;
		pragma Pack (Unaligned);
		Y : Unaligned := (ASCII.NUL, T'Value ("3.5"));
	begin
		pragma Assert (T'Image (Infinity) = " INF");
		pragma Assert (T'Image (-Infinity) = "-INF");
		pragma Assert (T'Image (NaN) = " NAN");
		pragma Assert (T'Image (-NaN) = "-NAN");
		pragma Assert (not (-NaN < 0.0)); -- comparison NaN is always False
		pragma Assert (Is_Negative (-1.0));
		pragma Assert (not Is_Negative (0.0));
		pragma Assert (not Is_Negative (+1.0));
		pragma Assert (T'Adjacent (X, Infinity) > X);
		pragma Assert (T'Adjacent (X, -Infinity) < X);
		pragma Assert (T'Floor (T'Adjacent (X, Infinity)) = X);
		pragma Assert (T'Ceiling (T'Adjacent (X, -Infinity)) = X);
		pragma Assert (T'Ceiling (X + 0.1) = 3.0);
		pragma Assert (T'Compose (Y.Value, -1) = 0.4375);
		pragma Assert (T'Copy_Sign (X, -1.0) = -2.0);
		pragma Assert (T'Exponent (X) = 2);
		pragma Assert (T'Floor (X - 0.1) = 1.0);
		pragma Assert (T'Fraction (X) = 0.5);
		pragma Assert (T'Leading_Part (X + 0.75, 3) = 2.5);
		pragma Assert (T'Machine (X) = X);
		pragma Assert (T'Machine_Rounding (X - 0.1) = X);
		pragma Assert (T'Model (X) = X);
		pragma Assert (T'Pred (X) < X);
		pragma Assert (T'Ceiling (T'Pred (X)) = X);
		pragma Assert (T'Remainder (Y.Value, 2.0) = -0.5);
		pragma Assert (T'Remainder (-Y.Value, 2.0) = 0.5);
		pragma Assert (T'Remainder (Y.Value, -2.0) = -0.5);
		pragma Assert (T'Remainder (-Y.Value, -2.0) = 0.5);
		pragma Assert (T'Rounding (X + 0.9) = 3.0);
		pragma Assert (T'Succ (X) > X);
		pragma Assert (T'Floor (T'Succ (X)) = X);
		pragma Assert (T'Truncation (X + 0.9) = 2.0);
		pragma Assert (T'Unbiased_Rounding (X + 0.5) = 2.0);
		pragma Assert (T'Unbiased_Rounding (X - 0.5) = 2.0);
		pragma Assert (X'Valid);
		pragma Assert (Y.Value'Valid); -- Unaligned_Valid
		null;
	end Test;
begin
	declare
		procedure Short_Float_Test is new Test (Short_Float);
	begin
		Short_Float_Test;
	end;
	declare
		procedure Float_Test is new Test (Float);
	begin
		Float_Test;
	end;
	declare
		procedure Long_Float_Test is new Test (Long_Float);
	begin
		Long_Float_Test;
	end;
	declare
		procedure Long_Long_Float_Test is new Test (Long_Long_Float);
	begin
		Long_Long_Float_Test;
	end;
	declare
		type Custom_Float is digits 12;
		procedure Custom_Float_Test is new Test (Custom_Float);
	begin
		Custom_Float_Test;
	end;
	Dividing : declare
		procedure Divide is new Ada.Float.Divide (
			Long_Long_Float,
			Long_Long_Float,
			Long_Long_Float,
			Long_Long_Float);
		Q, R : Long_Long_Float;
	begin
		Divide (4.5, 2.0, Q, R);
		pragma Assert (Q = 2.0 and then R = 0.5);
		Divide (5.0, 0.5, Q, R);
		pragma Assert (Q = 10.0 and then R = 0.0);
		Divide (0.9, 1.0, Q, R);
		pragma Assert (Q = 0.0 and then R = 0.9);
		Divide (-0.9, 1.0, Q, R);
		pragma Assert (Q = 0.0 and then R = -0.9);
	end Dividing;
	pragma Debug (Ada.Debug.Put ("OK"));
end floats;
