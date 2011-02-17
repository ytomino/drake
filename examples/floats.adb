with Ada.Float;
procedure floats is
	generic
		type T is digits <>;
	procedure Test;
	procedure Test is
		function Infinity is new Ada.Float.Infinity (T);
		function NaN is new Ada.Float.NaN (T);
		X : T := T'Value ("2.0");
		type R is record
			Padding : Character;
			Value : T;
		end record;
		pragma Pack (R);
		Y : R := (ASCII.NUL, T'Value ("3.5"));
	begin
		pragma Assert (T'Image (Infinity) = " INF");
		pragma Assert (T'Image (-Infinity) = "-INF");
		pragma Assert (T'Image (NaN) = " NAN");
		pragma Assert (T'Image (-NaN) = "-NAN");
		pragma Assert (not (-NaN < 0.0)); -- comparison NaN is always False
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
		pragma Assert (T'Remainder (Y.Value, 2.0) = 1.5);
		pragma Assert (T'Remainder (-Y.Value, 2.0) = -1.5);
		pragma Assert (T'Remainder (Y.Value, -2.0) = 1.5);
		pragma Assert (T'Remainder (-Y.Value, -2.0) = -1.5);
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
	procedure Float_Test is new Test (Float);
	procedure Long_Float_Test is new Test (Long_Float);
	procedure Long_Long_Float_Test is new Test (Long_Long_Float);
	procedure Short_Float_Test is new Test (Short_Float);
	type Custom_Float is digits 12;
	procedure Custom_Float_Test is new Test (Custom_Float);
begin
	pragma Debug (Float_Test);
	pragma Debug (Long_Float_Test);
	pragma Debug (Long_Long_Float_Test);
	pragma Debug (Short_Float_Test);
	pragma Debug (Custom_Float_Test);
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end floats;
