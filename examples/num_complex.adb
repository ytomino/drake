with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Short_Complex_Arrays;
with Ada.Numerics.Short_Complex_Elementary_Functions;
with Ada.Numerics.Short_Complex_Types;
with Ada.Numerics.Short_Real_Arrays;
with Ada.Numerics.Complex_Arrays;
with Ada.Numerics.Complex_Elementary_Functions;
with Ada.Numerics.Complex_Types;
with Ada.Numerics.Real_Arrays;
with Ada.Numerics.Long_Complex_Arrays;
with Ada.Numerics.Long_Complex_Elementary_Functions;
with Ada.Numerics.Long_Complex_Types;
with Ada.Numerics.Long_Real_Arrays;
with Ada.Numerics.Long_Long_Complex_Arrays;
with Ada.Numerics.Long_Long_Complex_Elementary_Functions;
with Ada.Numerics.Long_Long_Complex_Types;
with Ada.Numerics.Long_Long_Real_Arrays;
procedure num_complex is
	generic
		type T is digits <>;
		with package RA is new Ada.Numerics.Generic_Real_Arrays (T);
		with package CT is new Ada.Numerics.Generic_Complex_Types (T);
		with package CEF is new Ada.Numerics.Generic_Complex_Elementary_Functions (CT);
		with package CA is new Ada.Numerics.Generic_Complex_Arrays (RA, CT);
	procedure Test;
	procedure Test is
		use CT, CEF, CA;
		Pi_div_4 : constant Complex := Ada.Numerics.Pi / 4.0 + 0.0 * i;
	begin
		-- primitives of imaginary
		pragma Assert (+i = i);
		pragma Assert (-i = 1.0 / i);
		pragma Assert (abs i = 1.0);
		pragma Assert (i + i = -2.0 / i);
		pragma Assert (i - i = 0.0 / i);
		pragma Assert (2.0 * i = i + i);
		pragma Assert (i / i = 1.0);
		pragma Assert (i * 2.0 = i + i);
		pragma Assert (i / 2.0 = 0.5 * i);
		pragma Assert (2.0 / i = -2.0 * i);
		pragma Assert (i * i = -1.0);
		pragma Assert (i > -i and then -i < i);
		pragma Assert (i >= j and then j <= i);
		-- primitives of complex
		pragma Assert (Argument (1.0 + i) = Ada.Numerics.Pi / 4.0);
		pragma Assert (abs (1.0 + i) + 0.0 * i = Sqrt (2.0 + 0.0 * i));
		pragma Assert (Complex'(Conjugate (1.0 + i)) = 1.0 - i);
		pragma Assert (abs (
			Complex'(Compose_From_Polar (2.0, Ada.Numerics.Pi / 2.0))
			- Complex'(0.0 + 2.0 * i)) < 1.0e-5);
		pragma Assert (abs (
			Complex'((1.0 + i) ** 2)
			- Complex'(0.0 + 2.0 * i)) < 1.0e-5);
		pragma Assert (abs (
			Complex'(i ** 4)
			- Complex'(1.0 + 0.0 * i)) < 1.0e-5);
		-- elementary functions
		pragma Assert (abs (Arcsin (Sin (Pi_div_4)) - Pi_div_4) < 1.0e-5);
		pragma Assert (abs (Arccos (Cos (Pi_div_4)) - Pi_div_4) < 1.0e-5);
		pragma Assert (abs (Arctan (Tan (Pi_div_4)) - Pi_div_4) < 1.0e-5);
		pragma Assert (abs (Arccot (Cot (Pi_div_4)) - Pi_div_4) < 1.0e-5);
		-- arrays
		null;
	end Test;
begin
	declare
		procedure Short_Float_Test is
			new Test (
				Short_Float,
				Ada.Numerics.Short_Real_Arrays,
				Ada.Numerics.Short_Complex_Types,
				Ada.Numerics.Short_Complex_Elementary_Functions,
				Ada.Numerics.Short_Complex_Arrays);
	begin
		Short_Float_Test;
	end;
	declare
		procedure Float_Test is
			new Test (
				Float,
				Ada.Numerics.Real_Arrays,
				Ada.Numerics.Complex_Types,
				Ada.Numerics.Complex_Elementary_Functions,
				Ada.Numerics.Complex_Arrays);
	begin
		Float_Test;
	end;
	declare
		procedure Long_Float_Test is
			new Test (
				Long_Float,
				Ada.Numerics.Long_Real_Arrays,
				Ada.Numerics.Long_Complex_Types,
				Ada.Numerics.Long_Complex_Elementary_Functions,
				Ada.Numerics.Long_Complex_Arrays);
	begin
		Long_Float_Test;
	end;
	declare
		procedure Long_Long_Float_Test is
			new Test (
				Long_Long_Float,
				Ada.Numerics.Long_Long_Real_Arrays,
				Ada.Numerics.Long_Long_Complex_Types,
				Ada.Numerics.Long_Long_Complex_Elementary_Functions,
				Ada.Numerics.Long_Long_Complex_Arrays);
	begin
		Long_Long_Float_Test;
	end;
	declare
		type Custom_Float is digits 12;
		package Custom_Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Custom_Float);
		package Custom_Complex_Types is new Ada.Numerics.Generic_Complex_Types (Custom_Float);
		package Custom_Complex_Elementary_Functions is new Ada.Numerics.Generic_Complex_Elementary_Functions (Custom_Complex_Types);
		package Custom_Complex_Arrays is new Ada.Numerics.Generic_Complex_Arrays (Custom_Real_Arrays, Custom_Complex_Types);
		procedure Custom_Float_Test is
			new Test (
				Custom_Float,
				Custom_Real_Arrays,
				Custom_Complex_Types,
				Custom_Complex_Elementary_Functions,
				Custom_Complex_Arrays);
	begin
		Custom_Float_Test;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end num_complex;
