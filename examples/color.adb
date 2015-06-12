with Ada.Colors;
with Ada.Numerics;
procedure Color is
	use type Ada.Colors.RGB;
	P : constant := 16#0.0001#; -- 1/65536
begin
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
	for R in 0 .. 4 loop
		for G in 0 .. 4 loop
			for B in 0 .. 4 loop
				declare
					Original_RGB : constant Ada.Colors.RGB := (
						Red => Ada.Colors.Brightness'Base (R) / 4.0,
						Green => Ada.Colors.Brightness'Base (G) / 4.0,
						Blue => Ada.Colors.Brightness'Base (B) / 4.0);
					HSV : constant Ada.Colors.HSV := Ada.Colors.To_HSV (Original_RGB);
					RGB : constant Ada.Colors.RGB := Ada.Colors.To_RGB (HSV);
				begin
					pragma Assert (
						abs (RGB.Red - Original_RGB.Red) < P
						and then abs (RGB.Green - Original_RGB.Green) < P
						and then abs (RGB.Blue - Original_RGB.Blue) < P);
					pragma Assert (Ada.Colors.RGB_Distance (RGB, Original_RGB) < P);
					null;
				end;
			end loop;
		end loop;
	end loop;
	pragma Debug (Ada.Debug.Put ("OK"));
end Color;
