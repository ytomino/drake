with Ada.Decimal;
with Ada.Text_IO;
procedure fixed is
begin
	declare
		type T1 is delta 0.1 digits 10;
		procedure Divide is new Ada.Decimal.Divide (T1, T1, T1, T1);
		Q, R : T1;
	begin
		Divide (5.0, 0.5, Q, R);
		pragma Assert (Q = 10.0 and then R = 0.0);
		Divide (0.5, 1.2, Q, R);
		Ada.Text_IO.Put_Line (T1'Image (Q) & T1'Image (R));
		Ada.Text_IO.Put_Line (T1'Image (-1.2));
	end;
	declare
		type T2 is delta 1.0 digits 10;
		procedure Divide is new Ada.Decimal.Divide (T2, T2, T2, T2);
		Q2, R2 : T2;
	begin
		Divide (50.0, 12.0, Q2, R2);
		Ada.Text_IO.Put_Line (T2'Image (Q2) & T2'Image (R2));
		Ada.Text_IO.Put_Line (T2'Image (-12.0));
	end;
	declare
		type T3 is delta 10.0 digits 10;
		procedure Divide is new Ada.Decimal.Divide (T3, T3, T3, T3);
		Q3, R3 : T3;
	begin
		Divide (5000.0, 120.0, Q3, R3);
		Ada.Text_IO.Put_Line (T3'Image (Q3) & T3'Image (R3));
		Ada.Text_IO.Put_Line (T3'Image (-120.0));
	end;
end fixed;
