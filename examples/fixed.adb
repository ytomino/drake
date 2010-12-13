with Ada.Decimal;
with Ada.Text_IO;
procedure fixed is
begin
	-- deciaml fixed (power of 10)
	declare
		type T1 is delta 0.1 digits 10;
		pragma Assert (T1'Aft = 1);
		procedure Divide is new Ada.Decimal.Divide (T1, T1, T1, T1);
		Q, R : T1;
	begin
		Ada.Text_IO.Put_Line ("T1'Scale = " & Integer'Image (T1'Scale));
		Ada.Text_IO.Put_Line ("T1'Delta = " & Long_Long_Float'Image (T1'Delta));
		Ada.Text_IO.Put_Line ("T1'Small = " & Long_Long_Float'Image (T1'Small));
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
		Ada.Text_IO.Put_Line ("T2'Scale = " & Integer'Image (T2'Scale));
		Ada.Text_IO.Put_Line ("T2'Delta = " & Long_Long_Float'Image (T2'Delta));
		Ada.Text_IO.Put_Line ("T2'Small = " & Long_Long_Float'Image (T2'Small));
		Divide (50.0, 12.0, Q2, R2);
		Ada.Text_IO.Put_Line (T2'Image (Q2) & T2'Image (R2));
		Ada.Text_IO.Put_Line (T2'Image (-12.0));
	end;
	declare
		type T3 is delta 10.0 digits 10;
		procedure Divide is new Ada.Decimal.Divide (T3, T3, T3, T3);
		Q3, R3 : T3;
	begin
		Ada.Text_IO.Put_Line ("T3'Scale = " & Integer'Image (T3'Scale));
		Ada.Text_IO.Put_Line ("T3'Delta = " & Long_Long_Float'Image (T3'Delta));
		Ada.Text_IO.Put_Line ("T3'Small = " & Long_Long_Float'Image (T3'Small));
		Divide (5000.0, 120.0, Q3, R3);
		Ada.Text_IO.Put_Line (T3'Image (Q3) & T3'Image (R3));
		Ada.Text_IO.Put_Line (T3'Image (-120.0));
	end;
	-- ordinary fixed (power of 2)
	declare
		type T4 is delta 0.01 range 0.00 .. 1.99;
		pragma Assert (T4'Aft = 2);
	begin
		-- ordinary fixed type does not have 'Scale attribute.
		Ada.Text_IO.Put_Line ("T4'Delta = " & Long_Long_Float'Image (T4'Delta));
		Ada.Text_IO.Put_Line ("T4'Small = " & Long_Long_Float'Image (T4'Small));
		Ada.Text_IO.Put_Line (T4'Image (1.45));
	end;
	declare
		type T5 is delta 100.0 range -10000.0 .. 10000.0;
		pragma Assert (T5'Aft = 1);
	begin
		-- ordinary fixed type does not have 'Scale attribute.
		Ada.Text_IO.Put_Line ("T5'Delta = " & Long_Long_Float'Image (T5'Delta));
		Ada.Text_IO.Put_Line ("T5'Small = " & Long_Long_Float'Image (T5'Small));
		Ada.Text_IO.Put_Line (T5'Image (-128.0));
	end;
end fixed;
