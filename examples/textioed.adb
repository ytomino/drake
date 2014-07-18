with Ada.Text_IO.Editing;
with Ada.Wide_Text_IO.Editing;
with Ada.Wide_Wide_Text_IO.Editing;
procedure textioed is
	type T is delta 0.001 digits 12;
	package O is new Ada.Text_IO.Editing.Decimal_Output (T);
begin
	-- simple
	pragma Assert (
		O.Image (0.0, Ada.Text_IO.Editing.To_Picture ("999.999")) =
		"000.000");
	pragma Assert (
		O.Image (0.0, Ada.Text_IO.Editing.To_Picture ("ZZ9.999")) =
		"  0.000");
	pragma Assert (
		O.Image (0.0, Ada.Text_IO.Editing.To_Picture ("ZZZ.999")) =
		"   .000");
	pragma Assert (
		O.Image (1.0, Ada.Text_IO.Editing.To_Picture ("ZZZ_ZZ9")) =
		"      1");
	pragma Assert (
		O.Image (1000.0, Ada.Text_IO.Editing.To_Picture ("ZZZ_ZZ9")) =
		"  1,000");
	pragma Assert (
		O.Image (-10.0, Ada.Text_IO.Editing.To_Picture ("++9")) =
		"-10");
	pragma Assert (
		O.Image (10.0, Ada.Text_IO.Editing.To_Picture ("++9")) =
		"+10");
	pragma Assert (
		O.Image (10.0, Ada.Text_IO.Editing.To_Picture ("--9")) =
		" 10");
	pragma Assert (
		O.Image (10.0, Ada.Text_IO.Editing.To_Picture ("-9")) =
		"10");
	begin
		declare -- overflow
			S : constant String :=
				O.Image (1000.0, Ada.Text_IO.Editing.To_Picture ("--9"));
		begin
			null;
		end;
		raise Program_Error; -- bad
	exception
		when Ada.Text_IO.Layout_Error =>
			null;
	end;
	-- examples in RM F.3.2
	S_73_3 : declare
		Item : constant T := 123456.78;
		Pi : constant String := "-###**_***_**9.99";
		R1 : constant String := "   $***123,456.78";
		R2 : constant String := "  FF***123.456,78";
	begin
		pragma Assert (O.Image (Item, Ada.Text_IO.Editing.To_Picture (Pi)) = R1);
		pragma Assert (
			O.Image (
				Item,
				Ada.Text_IO.Editing.To_Picture (Pi),
				Currency => "FF",
				Separator => '.',
				Radix_Mark => ',') =
			R2);
		null;
	end S_73_3;
	S_74_1 : declare
		Item : constant T := 123456.78;
		Pi : constant String := "-$**_***_**9.99";
		R1 : constant String := " $***123,456.78";
		R2 : constant String := " FF***123.456,78";
	begin
		pragma Assert (O.Image (Item, Ada.Text_IO.Editing.To_Picture (Pi)) = R1);
		pragma Assert (
			O.Image (
				Item,
				Ada.Text_IO.Editing.To_Picture (Pi),
				Currency => "FF",
				Separator => '.',
				Radix_Mark => ',') =
			R2);
		null;
	end S_74_1;
	S_75 : declare
		Item : constant T := 0.0;
		Pi : constant String := "-$$$$$$.$$";
		R1 : constant String := "          ";
	begin
		pragma Assert (O.Image (Item, Ada.Text_IO.Editing.To_Picture (Pi)) = R1);
		null;
	end S_75;
	S_76 : declare
		Item : constant T := 0.20;
		Pi : constant String := "-$$$$$$.$$";
		R1 : constant String := "      $.20";
	begin
		pragma Assert (O.Image (Item, Ada.Text_IO.Editing.To_Picture (Pi)) = R1);
		null;
	end S_76;
	S_77 : declare
		Item : constant T := -1234.565;
		Pi : constant String := "<<<<_<<<.<<###>";
		R1 : constant String := "  (1,234.57DM )";
	begin
		pragma Assert (
			O.Image (
				Item,
				Ada.Text_IO.Editing.To_Picture (Pi),
				Currency => "DM") =
			R1);
		null;
	end S_77;
	S_78 : declare
		Item : constant T := 12345.67;
		Pi : constant String := "###_###_##9.99";
		R1 : constant String := "  CHF12,345.67";
	begin
		pragma Assert (
			O.Image (
				Item,
				Ada.Text_IO.Editing.To_Picture (Pi),
				Currency => "CHF") =
			R1);
		null;
	end S_78;
	pragma Debug (Ada.Debug.Put ("OK"));
end textioed;
