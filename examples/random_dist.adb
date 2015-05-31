with Ada.Float;
with Ada.Numerics.Distributions;
with Ada.Numerics.SFMT_216091;
with Ada.Text_IO;
procedure random_dist is
	package R renames Ada.Numerics.SFMT_216091;
	package D renames Ada.Numerics.Distributions;
	generic
		type Target is range <>;
		with procedure Process (X : out Target);
	procedure Generic_Check;
	procedure Generic_Check is
		Req : constant := 100;
		Box : array (Target) of Natural := (others => 0);
		N : Natural := 0;
		Max : Natural := 0;
	begin
		while N < Req * Target'Range_Length loop
			declare
				X : Target;
			begin
				Process (X);
				if Box (X) < Req then
					N := N + 1;
				end if;
				Box (X) := Box (X) + 1;
				if Box (X) > Max then
					Max := Box (X);
				end if;
			end;
		end loop;
		for I in Target loop
			Ada.Text_IO.Put_Line ("    " & (1 .. Box (I) * 40 / Max => '*'));
		end loop;
	end Generic_Check;
	Gen : aliased R.Generator := R.Initialize;
begin
	Ada.Text_IO.Put_Line ("Linear_Discrete");
	Ada.Text_IO.Put_Line ("  ==== 0 bit ====");
	declare
		type S is mod 10;
		type T is range 3 .. 3;
		function To is new D.Linear_Discrete (S, T);
		procedure Process (X : out T) is
		begin
			X := To (S'Mod (R.Random_32 (Gen)));
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("  ==== 1:1 ====");
	declare
		type S is mod 10;
		type T is range 2 .. 11;
		pragma Assert (S'Range_Length = T'Range_Length);
		function To is new D.Linear_Discrete (S, T);
		procedure Process (X : out T) is
		begin
			X := To (S'Mod (R.Random_32 (Gen)));
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("  ==== integer arithmetic ====");
	declare
		type S is mod 20;
		type T is range 2 .. 11;
		pragma Assert (S'Range_Length = T'Range_Length * 2);
		function To is new D.Linear_Discrete (S, T);
		procedure Process (X : out T) is
		begin
			X := To (S'Mod (R.Random_32 (Gen)));
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("  ==== float arithmetic ====");
	declare
		type S1 is mod 2 ** (Long_Long_Integer'Size / 2 + 1);
		type S2 is mod 2 ** (Long_Long_Integer'Size / 2);
		function To is new D.Linear_Discrete (S1, S2);
		type T is range 0 .. 7;
		procedure Process (X : out T) is
		begin
			X := T (To (S1'Mod (R.Random_64 (Gen))) mod 8);
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("Linear_Float_0_To_1");
	declare
		type S is mod 11;
		function To is new D.Linear_Float_0_To_1 (S, Long_Long_Float'Base);
		type T is range 0 .. 10;
		procedure Process (X : out T) is
		begin
			X := T (Long_Long_Float'Floor (To (S'Mod (R.Random_64 (Gen))) * 10.0));
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		pragma Assert (To (0) = 0.0);
		pragma Assert (To (5) = 0.5);
		pragma Assert (To (S'Last) = 1.0);
		Check;
	end;
	Ada.Text_IO.Put_Line ("Linear_Float_0_To_Less_Than_1");
	declare
		type S is mod 10;
		function To is new D.Linear_Float_0_To_Less_Than_1 (S, Long_Long_Float'Base);
	begin
		pragma Assert (To (0) = 0.0);
		pragma Assert (To (5) = 0.5);
		pragma Assert (To (S'Last) < 1.0);
		null;
	end;
	Ada.Text_IO.Put_Line ("Linear_Float_Greater_Than_0_To_Less_Than_1");
	declare
		type S is mod 9;
		function To is new D.Linear_Float_Greater_Than_0_To_Less_Than_1 (S, Long_Long_Float'Base);
	begin
		pragma Assert (To (0) > 0.0);
		pragma Assert (To (4) = 0.5);
		pragma Assert (To (S'Last) < 1.0);
		null;
	end;
	Ada.Text_IO.Put_Line ("Exponentially_Float");
	declare
		function To is new D.Exponentially_Float (R.Unsigned_32, Long_Long_Float'Base);
		type T is range 0 .. 9;
		procedure Process (X : out T) is
			function Is_Infinity is new Ada.Float.Is_Infinity (Long_Long_Float'Base);
			function Is_NaN is new Ada.Float.Is_NaN (Long_Long_Float'Base);
		begin
			loop
				declare
					F : Long_Long_Float'Base := To (R.Random_32 (Gen)) * 2.5;
				begin
					pragma Assert (not Is_Infinity (F));
					pragma Assert (not Is_NaN (F));
					if F < 10.0 then
						X := T (Long_Long_Float'Floor (F));
						exit;
					end if;
				end;
			end loop;
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("Uniform_Discrete_Random");
	Ada.Text_IO.Put_Line ("  ==== 0 bit ====");
	declare
		type T is range 3 .. 3;
		function Random is new D.Uniform_Discrete_Random (R.Unsigned_32, T, R.Generator, R.Random_32);
		procedure Process (X : out T) is
		begin
			X := Random (Gen);
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("  ==== 1:1 ====");
	declare
		type S2 is range
			Long_Long_Integer (R.Unsigned_32'First) + 1 ..
			Long_Long_Integer (R.Unsigned_32'Last) + 1;
		function Random is new D.Uniform_Discrete_Random (R.Unsigned_32, S2, R.Generator, R.Random_32);
		type T is range 0 .. 7;
		procedure Process (X : out T) is
		begin
			X := T (Random (Gen) mod 8);
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("  ==== narrow ====");
	declare
		type T is range 2 .. 9;
		function Random is new D.Uniform_Discrete_Random (R.Unsigned_64, T, R.Generator, R.Random_64);
		procedure Process (X : out T) is
		begin
			X := Random (Gen);
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("  ==== wide ====");
	declare
		type S2 is mod 2 ** (R.Unsigned_32'Size + 1);
		function Random is new D.Uniform_Discrete_Random (R.Unsigned_32, S2, R.Generator, R.Random_32);
		type T is range 0 .. 7;
		procedure Process (X : out T) is
		begin
			X := T (Random (Gen) mod 8);
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("Uniform_Float_Random_0_To_1");
	declare
		function Random is new D.Uniform_Float_Random_0_To_Less_Than_1 (R.Unsigned_32, Float'Base, R.Generator, R.Random_32);
		type T is range 0 .. 9;
		procedure Process (X : out T) is
			Z : T'Base;
		begin
			loop
				Z := T'Base (Float'Floor (Random (Gen) * 10.0));
				exit when Z /= 10; -- 1.0 is a rare case
			end loop;
			X := Z;
		end Process;
		procedure Check is new Generic_Check (T, Process);
	begin
		Check;
	end;
	Ada.Text_IO.Put_Line ("Uniform_Float_Random_0_To_Less_Than_1");
	declare
		function Random is new D.Uniform_Float_Random_0_To_Less_Than_1 (R.Unsigned_32, Long_Float'Base, R.Generator, R.Random_32);
	begin
		pragma Assert (Random (Gen) < 1.0);
		null;
	end;
	Ada.Text_IO.Put_Line ("Uniform_Float_Random_Greater_Than_0_To_Less_Than_1");
	declare
		function Random is new D.Uniform_Float_Random_Greater_Than_0_To_Less_Than_1 (R.Unsigned_32, Long_Long_Float'Base, R.Generator, R.Random_32);
		X : constant Long_Long_Float'Base := Random (Gen);
	begin
		pragma Assert (X > 0.0 and then X < 1.0);
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end random_dist;
