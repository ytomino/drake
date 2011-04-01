with Ada.Calendar;
with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
procedure cal is
	use type Ada.Calendar.Time;
	use type Ada.Calendar.Time_Zones.Time_Offset;
	Now : Ada.Calendar.Time := Ada.Calendar.Clock;
	Year : Ada.Calendar.Year_Number;
	Month : Ada.Calendar.Month_Number;
	Day : Ada.Calendar.Day_Number;
	Seconds : Ada.Calendar.Day_Duration;
	H : Ada.Calendar.Formatting.Hour_Number;
	M : Ada.Calendar.Formatting.Minute_Number;
	S : Ada.Calendar.Formatting.Second_Number;
	SS : Ada.Calendar.Formatting.Second_Duration;
	LS : Boolean;
	procedure printf (format : String; Time : Ada.Calendar.Time);
	procedure printf (format : String; Y, M, D : Integer; S : Duration);
	procedure printf (format : String; Y, Mo, D, H, Mi, S : Integer; SS : Duration);
	procedure printf (format : String; Offset : Ada.Calendar.Time_Zones.Time_Offset);
	procedure printf (format : String; WD : Integer);
	pragma Import (C, printf);
	Remaked : Ada.Calendar.Time;
begin
	pragma Debug (Ada.Debug.Put ("assertion enabled"));
	printf ("%16llx" & ASCII.LF & ASCII.NUL, Now);
	Ada.Calendar.Split (Now, Year, Month, Day, Seconds);
	Ada.Calendar.Formatting.Split (Seconds, H, M, S, SS);
	printf ("GM %d-%d-%d %lld" & ASCII.LF & ASCII.NUL, Year, Month, Day, Seconds);
	printf ("GM %d-%d-%d %d:%d:%d %lld" & ASCII.LF & ASCII.NUL, Year, Month, Day, H, M, S, SS);
	pragma Assert (Ada.Calendar.Seconds (Now) = Seconds);
	Remaked := Ada.Calendar.Time_Of (Year, Month, Day, Seconds);
	pragma Assert (Remaked = Now);
	printf ("TZ = %d" & ASCII.LF & ASCII.NUL, Ada.Calendar.Time_Zones.UTC_Time_Offset);
	Ada.Debug.Put (Ada.Calendar.Formatting.Image (Ada.Calendar.Time_Zones.UTC_Time_Offset));
	pragma Assert (Ada.Calendar.Formatting.Value (
		Ada.Calendar.Formatting.Image (Ada.Calendar.Time_Zones.UTC_Time_Offset)) =
		Ada.Calendar.Time_Zones.UTC_Time_Offset);
	-- Time_Offset = 540 in Japan
	Ada.Calendar.Formatting.Split (Now, Year, Month, Day, H, M, S, SS, LS,
		Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset);
	printf ("LT %d-%d-%d %d:%d:%d %lld" & ASCII.LF & ASCII.NUL, Year, Month, Day, H, M, S, SS);
	Remaked := Ada.Calendar.Formatting.Time_Of (Year, Month, Day, H, M, S, SS, LS,
		Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset);
	pragma Assert (Remaked = Now);
	printf ("%d" & ASCII.LF & ASCII.NUL, Integer'(Ada.Calendar.Formatting.Day_Name'Pos (
		Ada.Calendar.Formatting.Day_of_Week (Now,
		Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset))));
	declare
		Img : String := Ada.Calendar.Formatting.Image (Now, Include_Time_Fraction => True);
	begin
		Ada.Debug.Put (Img);
		Remaked := Ada.Calendar.Formatting.Value (Img);
		Ada.Debug.Put (Ada.Calendar.Formatting.Image (Remaked, Include_Time_Fraction => True));
	end;
	declare
		X : Duration := Duration'(((15.0 * 60.0) + 25.0) * 60.0 + 35.45);
		Img : String := Ada.Calendar.Formatting.Image (X, Include_Time_Fraction => True);
		RX : Duration;
	begin
		Ada.Debug.Put (Img);
		RX := Ada.Calendar.Formatting.Value (Img);
		Ada.Debug.Put (Ada.Calendar.Formatting.Image (RX, Include_Time_Fraction => True));
		pragma Assert (RX = X);
	end;
	declare
		EF : Ada.Calendar.Time;
		EL : Ada.Calendar.Time;
	begin
		EF := Ada.Calendar.Time_Of (1901, 1, 1);
		Ada.Calendar.Formatting.Split (EF, Year, Month, Day, H, M, S, SS, LS);
		printf ("EF %d-%d-%d %d:%d:%d %lld" & ASCII.LF & ASCII.NUL, Year, Month, Day, H, M, S, SS);
		EL := Ada.Calendar.Time_Of (2099, 12, 31);
		Ada.Calendar.Formatting.Split (EL, Year, Month, Day, H, M, S, SS, LS);
		printf ("EL %d-%d-%d %d:%d:%d %lld" & ASCII.LF & ASCII.NUL, Year, Month, Day, H, M, S, SS);
	exception
		when Ada.Calendar.Time_Error => Ada.Debug.Put ("Time_Error");
	end;
	declare
		D : Ada.Calendar.Time;
	begin
		D := Now - Duration'First + Duration'Last; -- out of range
		Ada.Debug.Put ("plase compile with -gnato");
	exception
		when Ada.Calendar.Time_Error => Ada.Debug.Put ("OK");
	end;
end cal;
