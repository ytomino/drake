with Ada.Colors;
with Ada.Numerics;
procedure Color is
	use type Ada.Colors.RGB;
	P : constant := 16#0.0001#; -- 1/65536
begin
	-- distances in HSV
	pragma Assert (
		abs (
			Ada.Colors.HSV_Distance (
				(Hue => 0.0, Saturation => 0.0, Value => 1.0),
				(Hue => Ada.Numerics.Pi / 2.0, Saturation => 0.0, Value => 1.0))
			- 0.0) < P);
	pragma Assert (
		abs (
			Ada.Colors.HSV_Distance (
				(Hue => 0.0, Saturation => 1.0, Value => 1.0),
				(Hue => Ada.Numerics.Pi / 2.0, Saturation => 1.0, Value => 1.0))
			- 0.5) < P); -- Sqrt (2.0) ** 2 / 2.0 = 0.5
	pragma Assert (
		abs (
			Ada.Colors.HSV_Distance (
				(Hue => 0.0, Saturation => 1.0, Value => 1.0),
				(Hue => Ada.Numerics.Pi, Saturation => 1.0, Value => 1.0))
			- 1.0) < P);
	-- distances in HSL
	pragma Assert (
		abs (
			Ada.Colors.HSL_Distance (
				(Hue => 0.0, Saturation => 1.0, Lightness => 1.0),
				(Hue => Ada.Numerics.Pi / 2.0, Saturation => 1.0, Lightness => 1.0))
			- 0.0) < P);
	pragma Assert (
		abs (
			Ada.Colors.HSL_Distance (
				(Hue => 0.0, Saturation => 1.0, Lightness => 0.5),
				(Hue => Ada.Numerics.Pi / 2.0, Saturation => 1.0, Lightness => 0.5))
			- 0.5) < P); -- Sqrt (2.0) ** 2 / 2.0 = 0.5
	pragma Assert (
		abs (
			Ada.Colors.HSL_Distance (
				(Hue => 0.0, Saturation => 1.0, Lightness => 0.0),
				(Hue => Ada.Numerics.Pi / 2.0, Saturation => 1.0, Lightness => 0.0))
			- 0.0) < P);
	-- conversions
	for R in 0 .. 4 loop
		for G in 0 .. 4 loop
			for B in 0 .. 4 loop
				declare
					Original_RGB : constant Ada.Colors.RGB := (
						Red => Ada.Colors.Brightness'Base (R) / 4.0,
						Green => Ada.Colors.Brightness'Base (G) / 4.0,
						Blue => Ada.Colors.Brightness'Base (B) / 4.0);
					HSV : constant Ada.Colors.HSV := Ada.Colors.To_HSV (Original_RGB);
					HSV_RGB : constant Ada.Colors.RGB := Ada.Colors.To_RGB (HSV);
					HSV_HSL : constant Ada.Colors.HSL := Ada.Colors.To_HSL (HSV);
					HSL : constant Ada.Colors.HSL := Ada.Colors.To_HSL (Original_RGB);
					HSL_RGB : constant Ada.Colors.RGB := Ada.Colors.To_RGB (HSL);
					HSL_HSV : constant Ada.Colors.HSV := Ada.Colors.To_HSV (HSL);
				begin
					-- RGB / HSV
					pragma Assert (
						abs (HSV_RGB.Red - Original_RGB.Red) < P
						and then abs (HSV_RGB.Green - Original_RGB.Green) < P
						and then abs (HSV_RGB.Blue - Original_RGB.Blue) < P);
					pragma Assert (Ada.Colors.RGB_Distance (HSV_RGB, Original_RGB) < P);
					-- RGB / HSL
					pragma Assert (
						abs (HSL_RGB.Red - Original_RGB.Red) < P
						and then abs (HSL_RGB.Green - Original_RGB.Green) < P
						and then abs (HSL_RGB.Blue - Original_RGB.Blue) < P);
					pragma Assert (Ada.Colors.RGB_Distance (HSL_RGB, Original_RGB) < P);
					-- HSV / HSL
					pragma Assert (
						abs (HSL_HSV.Hue - HSV.Hue) < P
						and then abs (HSL_HSV.Saturation - HSV.Saturation) < P
						and then abs (HSL_HSV.Value - HSV.Value) < P);
					pragma Assert (
						abs (HSV_HSL.Hue - HSL.Hue) < P
						and then abs (HSV_HSL.Saturation - HSL.Saturation) < P
						and then abs (HSV_HSL.Lightness - HSL.Lightness) < P);
					null;
				end;
			end loop;
		end loop;
	end loop;
	pragma Debug (Ada.Debug.Put ("OK"));
end Color;
