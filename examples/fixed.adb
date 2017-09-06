-- Demonstration of the extended units: Ada.Fixed
with Ada.Fixed;
with System;
procedure fixed is
begin
	declare -- Divide
		type T is delta System.Fine_Delta range 0.0 .. 1.0 - System.Fine_Delta;
		type Q_Type is delta 0.1 range 0.0 .. 999.0;
		procedure Divide is new Ada.Fixed.Divide (T, T, Q_Type, T);
		Q : Q_Type;
		R : T;
	begin
		pragma Assert (T'Small = T'(System.Fine_Delta));
		Divide (10 * T'Small, 3 * T'Small, Q, R);
		pragma Assert (Q = 3.0 and then R = T'Small);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end fixed;
