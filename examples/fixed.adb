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
		pragma Assert (Q = 0.4 and then R = 0.0); -- R = 0.02
		pragma Assert (T1'Image (-1.2) = "-1.2");
		pragma Assert (T1'Mantissa = 34); -- log 1e10 / log 2 = 33.2...
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
		pragma Assert (Q2 = 4.0 and then R2 = 2.0);
		pragma Assert (T2'Image (-12.0) = "-12.0");
		pragma Assert (T2'Mantissa = 34);
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
		pragma Assert (Q3 = 40.0 and then R3 = 200.0);
		pragma Assert (T3'Image (-120.0) = "-120.0");
		pragma Assert (T3'Mantissa = 34);
	end;
	-- ordinary fixed (power of 2)
	declare
		type T4 is delta 0.01 range 0.00 .. 1.99;
		pragma Assert (T4'Aft = 2);
	begin
		-- ordinary fixed type does not have 'Scale attribute.
		Ada.Text_IO.Put_Line ("T4'Delta = " & Long_Long_Float'Image (T4'Delta));
		Ada.Text_IO.Put_Line ("T4'Small = " & Long_Long_Float'Image (T4'Small));
		pragma Assert (T4'Image (1.45) = " 1.45");
		pragma Assert (T4'Mantissa = 8); -- log 199 / log 2 = 7.6...
	end;
	declare
		type T5 is delta 100.0 range -10000.0 .. 10000.0;
		pragma Assert (T5'Aft = 1);
	begin
		-- ordinary fixed type does not have 'Scale attribute.
		Ada.Text_IO.Put_Line ("T5'Delta = " & Long_Long_Float'Image (T5'Delta));
		Ada.Text_IO.Put_Line ("T5'Small = " & Long_Long_Float'Image (T5'Small));
		pragma Assert (T5'Image (-128.0) = "-128.0");
		pragma Assert (T5'Mantissa in 7 .. 8); -- log 100 / log 2 = 6.6...
		-- but T5'Mantissa = 8, not 7. errors in calculation?
	end;
	-- just 64bit
	declare
		type T6 is delta 0.25 range 0.0 .. 16#0fff_ffff_ffff_ffff.c#;
		X : T6 := T6'Last;
		Y : T6 := X / T6'Value (T6'Image (T6'Last));
	begin
		pragma Assert (Y = 1.0);
		pragma Assert (T6'Mantissa = 62);
		null;
	end;
	-- over 64bit
	pragma Assert (Duration'Mantissa = Duration'Size - 1);
	declare -- X > Y
		X : Duration := 16#1_ffff_ffff.0#;
		Y : Duration := X / Duration'Value ("2.0");
	begin
		pragma Assert (Y = 16#0_ffff_ffff.8#);
		null;
	end;
	declare -- X < X
		X : Duration := 16#1_0000_0000.0#;
		Y : Duration := X / Duration'Value ("16#2_0000_0000.0#");
	begin
		pragma Assert (Y = 0.5);
		null;
	end;
	-- dynamic range
	declare
		type T is delta 1.0 range -256.0 .. 255.0;
		subtype S0 is T range 0.0 .. T'Value ("0.0");
		subtype S1 is T range 0.0 .. T'Value ("1.0");
		subtype S6 is T range 0.0 .. T'Value ("63.0");
		subtype S7 is T range 0.0 .. T'Value ("64.0");
		subtype M6 is T range T'Value ("-64.0") .. 0.0;
		subtype M7 is T range T'Value ("-65.0") .. 0.0;
	begin
		pragma Assert (T'Mantissa = 8);
		pragma Assert (S0'Mantissa = 0);
		pragma Assert (S1'Mantissa = 1);
		pragma Assert (S6'Mantissa = 6);
		pragma Assert (S7'Mantissa = 7);
		pragma Assert (M6'Mantissa = 6);
		pragma Assert (M7'Mantissa = 7);
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end fixed;
