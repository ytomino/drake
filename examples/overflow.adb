with Ada;
with Interfaces;
procedure overflow is
	Arithmetic_64 : constant Duration :=
		Duration'Value ("1.0") / Duration'Value ("1.0"); -- link s-arit64
	generic
		type T is range <>;
	procedure Generic_Check;
	procedure Generic_Check is
		pragma Unsuppress (Overflow_Check);
		function Ident (X : T) return T is
		begin
			return X;
		end Ident;
		X : T;
	begin
		-- add
		begin
			X := Ident (T'Last - 1) + T'Value ("1");
			X := Ident (T'First + 1) + T'Value ("-1");
		exception
			when Constraint_Error =>
				pragma Assert (False);
				null;
		end;
		begin
			X := Ident (T'Last) + T'Value ("1");
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
		begin
			X := Ident (T'First) + T'Value ("-1");
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
		-- subtract
		begin
			X := Ident (T'First + 1) - T'Value ("1");
			X := Ident (T'Last - 1) - T'Value ("-1");
		exception
			when Constraint_Error =>
				pragma Assert (False);
				null;
		end;
		begin
			X := Ident (T'First) - T'Value ("1");
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
		begin
			X := Ident (T'Last) - T'Value ("-1");
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
		-- multiply
		begin
			X := T'Value ("0") * T'Value ("0"); -- 00
			X := T'Value ("0") * Ident (T'First); -- 0N
			X := Ident (T'First) * T'Value ("0"); -- N0
			X := T'Value ("0") * Ident (T'Last); -- 0P
			X := Ident (T'Last) * T'Value ("0"); -- P0
			X := Ident (T'First) * T'Value ("1"); -- NP
			X := T'Value ("1") * Ident (T'First); -- PN
			X := Ident (T'Last) * T'Value ("1"); -- PP
			X := Ident (-T'Last) * T'Value ("-1"); -- NN
		exception
			when Constraint_Error =>
				pragma Assert (False);
				null;
		end;
		begin
			X := Ident (T'First) * T'Value ("-1");
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
		begin
			X := T'Value ("-1") * Ident (T'First);
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
		begin
			X := Ident (T'First) * T'Value ("2");
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
		begin
			X := T'Value ("2") * Ident (T'First);
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
		begin
			X := Ident (T'Last) * T'Value ("2");
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
		begin
			X := T'Value ("2") * Ident (T'Last);
			pragma Assert (False);
		exception
			when Constraint_Error => null;
		end;
	end Generic_Check;
begin
	-- signed integers
	declare
		procedure Check_I8 is new Generic_Check (Interfaces.Integer_8);
	begin
		Check_I8;
	end;
	declare
		procedure Check_I16 is new Generic_Check (Interfaces.Integer_16);
	begin
		Check_I16;
	end;
	declare
		procedure Check_I32 is new Generic_Check (Interfaces.Integer_32);
	begin
		Check_I32;
	end;
	declare
		procedure Check_I64 is new Generic_Check (Interfaces.Integer_64);
	begin
		Check_I64;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end overflow;
