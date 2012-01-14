with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Numerics.MT19937;
procedure random is
begin
	-- reset with a fixed seed
	declare
		type T is range 1 .. 100;
		package R is new Ada.Numerics.Discrete_Random (T);
		use type R.State;
		Gen : R.Generator;
		State : R.State;
	begin
		R.Reset (Gen, Integer'(0));
		R.Save (Gen, State);
		declare
			Image : constant String := R.Image (State);
			State_2 : R.State := R.Value (Image);
		begin
			pragma Debug (Ada.Debug.Put (Image));
			pragma Assert (State_2 = State);
			null;
		end;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end random;
