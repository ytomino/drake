with Ada.Calendar;
with Ada.Real_Time;
procedure delays is
	use type Ada.Calendar.Time;
	use type Ada.Real_Time.Time;
	use type Ada.Real_Time.Time_Span;
begin
	declare
		Start : Ada.Calendar.Time := Ada.Calendar.Clock;
	begin
		delay 1.0;
		delay until Ada.Calendar.Clock + 1.0;
		pragma Assert (Ada.Calendar.Clock - Start >= 2.0);
		pragma Assert (Ada.Calendar.Clock - Start < 3.0); -- ??
	end;
	declare
		Start : Ada.Real_Time.Time := Ada.Real_Time.Clock;
	begin
		delay 1.0;
		delay until Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (1.0);
		pragma Assert (Ada.Real_Time.Clock - Start >= Ada.Real_Time.To_Time_Span (2.0));
		pragma Assert (Ada.Real_Time.Clock - Start < Ada.Real_Time.To_Time_Span (3.0)); -- ??
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end delays;
