with Ada.Colors;
with Ada.Text_IO.Terminal.Colors.Names;
procedure tty_color is
	use type Ada.Text_IO.Terminal.Colors.Color_Parameter;
	package CN
		renames Ada.Text_IO.Terminal.Colors.Names;
	System_Colors : constant array (0 .. 15) of Ada.Text_IO.Terminal.Colors.Color := (
		CN.Black,
		CN.Dark_Blue,
		CN.Dark_Green,
		CN.Dark_Cyan,
		CN.Dark_Red,
		CN.Dark_Magenta,
		CN.Dark_Yellow,
		CN.Gray,
		CN.Dark_Gray,
		CN.Blue,
		CN.Green,
		CN.Cyan,
		CN.Red,
		CN.Magenta,
		CN.Yellow,
		CN.White);
	Fullwidth_A : constant String := (
		Character'Val (16#EF#),
		Character'Val (16#BC#),
		Character'Val (16#A1#));
	Output : Ada.Text_IO.File_Type
		renames Ada.Text_IO.Standard_Output.all;
begin
	for I in System_Colors'Range loop
		Ada.Text_IO.Terminal.Colors.Set_Color (Output,
			Foreground => +System_Colors (System_Colors'Last - I),
			Background => +System_Colors (I));
		Ada.Text_IO.Put (Output, Fullwidth_A);
	end loop;
	Ada.Text_IO.Terminal.Colors.Reset_Color (Output);
	Ada.Text_IO.New_Line (Output, Spacing => 2);
	for R1 in 0 .. 1 loop -- large Y block
		for G in 0 .. 7 loop -- Y
			for R2 in 0 .. 3 loop -- large X block
				for B in 0 .. 7 loop -- X
					declare
						C : constant Ada.Text_IO.Terminal.Colors.Color :=
							Ada.Text_IO.Terminal.Colors.To_Color (
								Ada.Colors.RGB'(
									Red => Float (R1 * 4 + R2) / 7.0,
									Blue => Float (B) / 7.0,
									Green => Float (G) / 7.0));
					begin
						Ada.Text_IO.Terminal.Colors.Set_Color (Output, Background => +C);
						Ada.Text_IO.Put (Output, "  ");
					end;
				end loop;
				Ada.Text_IO.Terminal.Colors.Reset_Color (Output);
				if R2 < 3 then
					Ada.Text_IO.Put (Output, "  ");
				end if;
			end loop;
			Ada.Text_IO.New_Line (Output);
		end loop;
		if R1 < 1 then
			Ada.Text_IO.New_Line (Output);
		end if;
	end loop;
end tty_color;
