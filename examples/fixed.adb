with Ada.Decimal;
procedure fixed is
	type T is delta 0.1 digits 10;
	procedure Divide is new Ada.Decimal.Divide (T, T, T, T);
	Q, R : T;
begin
	Divide (5.0, 0.5, Q, R);
	pragma Assert (Q = 10.0 and then R = 0.0);
end fixed;
