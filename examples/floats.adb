-- Demonstration of the extended units: Ada.Fixed
with Ada.Float;
procedure floats is
	subtype T is Long_Long_Float;
begin
	declare -- Infinity and Is_Infinity
		function Infinity is new Ada.Float.Infinity (T);
		function Is_Infinity is new Ada.Float.Is_Infinity (T);
	begin
		pragma Assert (T'Image (Infinity) = " INF");
		pragma Assert (T'Image (-Infinity) = "-INF");
		pragma Assert (Is_Infinity (Infinity));
		null;
	end;
	declare -- NaN and Is_NaN
		function NaN is new Ada.Float.NaN (T);
		function Is_NaN is new Ada.Float.Is_NaN (T);
	begin
		pragma Assert (T'Image (NaN) = " NAN");
		pragma Assert (T'Image (-NaN) = "-NAN");
		pragma Assert (not (-NaN < 0.0)); -- comparison of NaN is always False
		pragma Assert (Is_NaN (NaN));
		null;
	end;
	declare -- Is_Negative
		function Is_Negative is new Ada.Float.Is_Negative (T);
	begin
		pragma Assert (Is_Negative (-1.0));
		pragma Assert (not Is_Negative (0.0));
		pragma Assert (not Is_Negative (+1.0));
		null;
	end;
	declare -- Divide
		procedure Divide is new Ada.Float.Divide (T, T, T, T);
		Q, R : T;
	begin
		Divide (4.5, 2.0, Q, R);
		pragma Assert (Q = 2.0 and then R = 0.5);
		Divide (5.0, 0.5, Q, R);
		pragma Assert (Q = 10.0 and then R = 0.0);
		Divide (0.9, 1.0, Q, R);
		pragma Assert (Q = 0.0 and then R = 0.9);
		Divide (-0.9, 1.0, Q, R);
		pragma Assert (Q = 0.0 and then R = -0.9);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end floats;
