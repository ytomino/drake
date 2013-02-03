with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Long_Long_Complex_Types; -- linking check
with Ada.Numerics.Long_Long_Complex_Elementary_Functions; -- linking check
procedure num_complex is
	generic
		type T is digits <>;
	procedure Test;
	procedure Test is
		package RA is new Ada.Numerics.Generic_Real_Arrays (T);
		package CT is new Ada.Numerics.Generic_Complex_Types (T);
		package CEF is new Ada.Numerics.Generic_Complex_Elementary_Functions (CT);
		package CA is new Ada.Numerics.Generic_Complex_Arrays (RA, CT);
		use CT, CEF, CA;
		Pi_div_4 : constant Complex := Ada.Numerics.Pi / 4.0 + 0.0 * i;
	begin
		-- primitives of imaginary
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
	pragma Debug (Ada.Debug.Put ("OK"));
end num_complex;
