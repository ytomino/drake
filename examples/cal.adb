with Ada.Calendar;
with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
procedure cal is
	type Integer_Time is mod 2 ** Duration'Size;
	pragma Warnings (Off, "representation of Time values may change between GNAT versions");
	function To_Integer is new Ada.Unchecked_Conversion (Ada.Calendar.Time, Integer_Time);
	pragma Warnings (On, "representation of Time values may change between GNAT versions");
	package Integer_Time_IO is new Ada.Text_IO.Modular_IO (Integer_Time);
	package Duration_IO is new Ada.Text_IO.Fixed_IO (Duration);
	package Time_Offset_IO is new Ada.Text_IO.Integer_IO (Ada.Calendar.Time_Zones.Time_Offset);
	use Ada.Text_IO, Ada.Integer_Text_IO, Integer_Time_IO, Duration_IO, Time_Offset_IO;
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
	Day_Name : Ada.Calendar.Formatting.Day_Name;
	Remaked : Ada.Calendar.Time;
begin
	pragma Debug (Ada.Debug.Put ("assertion enabled"));
	Put (To_Integer (Now), Base => 16); New_Line;
	Ada.Calendar.Split (Now, Year, Month, Day, Seconds);
	Ada.Calendar.Formatting.Split (Seconds, H, M, S, SS);
	Output_GM : begin
		Put ("GM ");
		Put (Year, Width => 4); Put ('-'); Put (Month, Width => 2); Put ('-'); Put (Day, Width => 2); Put (' ');
		Put (Seconds, Fore => 1); New_Line;
	end Output_GM;
	Output_GM_Full : begin
		Put ("GM ");
		Put (Year, Width => 4); Put ('-'); Put (Month, Width => 2); Put ('-'); Put (Day, Width => 2); Put (' ');
		Put (H, Width => 2); Put (':'); Put (M, Width => 2); Put (':'); Put (S, Width => 2); Put (' ');
		Put (SS, Fore => 1); New_Line;
	end Output_GM_Full;
	pragma Assert (Ada.Calendar.Seconds (Now) = Seconds);
	Remaked := Ada.Calendar.Time_Of (Year, Month, Day, Seconds);
	pragma Assert (Remaked = Now);
	PUt ("TZ = "); Put (Ada.Calendar.Time_Zones.UTC_Time_Offset, Width => 1); New_Line;
	Ada.Debug.Put (Ada.Calendar.Formatting.Image (Ada.Calendar.Time_Zones.UTC_Time_Offset));
	pragma Assert (Ada.Calendar.Formatting.Value (
		Ada.Calendar.Formatting.Image (Ada.Calendar.Time_Zones.UTC_Time_Offset)) =
		Ada.Calendar.Time_Zones.UTC_Time_Offset);
	-- Time_Offset = 540 in Japan
	Ada.Calendar.Formatting.Split (Now, Year, Month, Day, H, M, S, SS, LS,
		Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset);
	Output_LT_Full : begin
		Put ("LT ");
		Put (Year, Width => 4); Put ('-'); Put (Month, Width => 2); Put ('-'); Put (Day, Width => 2); Put (' ');
		Put (H, Width => 2); Put (':'); Put (M, Width => 2); Put (':'); Put (S, Width => 2); Put (' ');
		Put (SS, Fore => 1); New_Line;
	end Output_LT_Full;
	Remaked := Ada.Calendar.Formatting.Time_Of (Year, Month, Day, H, M, S, SS, LS,
		Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset);
	pragma Assert (Remaked = Now);
	Day_Name := Ada.Calendar.Formatting.Day_of_Week (
		Now,
		Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset);
	Put (Ada.Calendar.Formatting.Day_Name'Image (Day_Name)); New_Line;
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
	declare -- elapsed time
		Max : constant Duration := (99.0 * 60.0 + 59.0) * 60.0 + 59.99;
		Max_S : constant String := Ada.Calendar.Formatting.Image (Max, Include_Time_Fraction => True);
		Neg : constant Duration := -((50.0 * 60.0 + 30.0) * 60.0 + 30.0);
		Neg_S : constant String := Ada.Calendar.Formatting.Image (Neg, Include_Time_Fraction => False);
	begin
		Ada.Debug.Put (Max_S);
		pragma Assert (Max_S = "99:59:59.99");
		pragma Assert (Ada.Calendar.Formatting.Value (Max_S) = Max);
		Ada.Debug.Put (Neg_S);
		pragma Assert (Neg_S = "-50:30:30");
		pragma Assert (Ada.Calendar.Formatting.Value (Neg_S) = Neg);
	end;
	declare -- first / last
		EF : Ada.Calendar.Time;
		EL : Ada.Calendar.Time;
	begin
		EF := Ada.Calendar.Time_Of (1901, 1, 1);
		Ada.Calendar.Formatting.Split (EF, Year, Month, Day, H, M, S, SS, LS);
		Output_EF_Full : begin
			Put ("EF ");
			Put (Year, Width => 4); Put ('-'); Put (Month, Width => 2); Put ('-'); Put (Day, Width => 2); Put (' ');
			Put (H, Width => 2); Put (':'); Put (M, Width => 2); Put (':'); Put (S, Width => 2); Put (' ');
			Put (SS, Fore => 1); New_Line;
		end Output_EF_Full;
		EL := Ada.Calendar.Time_Of (2099, 12, 31);
		Ada.Calendar.Formatting.Split (EL, Year, Month, Day, H, M, S, SS, LS);
		Output_EL_Full : begin
			Put ("EL ");
			Put (Year, Width => 4); Put ('-'); Put (Month, Width => 2); Put ('-'); Put (Day, Width => 2); Put (' ');
			Put (H, Width => 2); Put (':'); Put (M, Width => 2); Put (':'); Put (S, Width => 2); Put (' ');
			Put (SS, Fore => 1); New_Line;
		end Output_EL_Full;
	exception
		when Ada.Calendar.Time_Error => Ada.Debug.Put ("Time_Error");
	end;
	declare -- overflow
		D : Ada.Calendar.Time;
	begin
		D := Now - Duration'First + Duration'Last; -- out of range
		Ada.Debug.Put ("plase compile with -gnato");
	exception
		when Ada.Calendar.Time_Error => null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end cal;
