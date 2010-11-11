with Ada.Calendar;
procedure delays is
	use type Ada.Calendar.Time;
begin
	delay 1.0;
	delay until Ada.Calendar.Clock + 1.0;
end delays;
