with Ada.Calendar;
procedure delays is
	use type Ada.Calendar.Time;
	Start : Ada.Calendar.Time := Ada.Calendar.Clock;
begin
	delay 1.0;
	delay until Ada.Calendar.Clock + 1.0;
	pragma Assert (Ada.Calendar.Clock - Start >= 2.0);
	pragma Assert (Ada.Calendar.Clock - Start < 3.0); -- ??
	Ada.Debug.Put ("OK");
end delays;
