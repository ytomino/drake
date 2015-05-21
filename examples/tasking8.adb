with Ada.Real_Time;
procedure tasking8 is
	use type Ada.Real_Time.Time_Span;
	S, E : Ada.Real_Time.Time;
begin
	-- normal
	S := Ada.Real_Time.Clock;
	declare
		task T1;
		task body T1 is
		begin
			delay until S + Ada.Real_Time.To_Time_Span (2.0);
		end T1;
	begin
		null;
	end;
	E := Ada.Real_Time.Clock;
	pragma Assert (
		abs (E - S - Ada.Real_Time.To_Time_Span (2.0)) <
		Ada.Real_Time.To_Time_Span (0.1));
	-- abort
	S := Ada.Real_Time.Clock;
	declare
		task T2;
		task body T2 is
		begin
			delay until S + Ada.Real_Time.To_Time_Span (2.0);
		end T2;
	begin
		delay 0.95; -- note, currently, Abort_Checking_Span = 1.0
		abort T2;
	end;
	E := Ada.Real_Time.Clock;
	pragma Assert (
		abs (E - S - Ada.Real_Time.To_Time_Span (1.0)) <
		Ada.Real_Time.To_Time_Span (0.1));
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking8;
