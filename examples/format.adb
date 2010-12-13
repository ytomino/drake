with Ada.Formatting;
procedure format is
begin
	declare
		function Image is new Ada.Formatting.Integer_Image (Integer);
		function Trimed_Image is new Ada.Formatting.Integer_Image (
			Integer,
			Zero_Sign => Ada.Formatting.None,
			Plus_Sign => Ada.Formatting.None);
		function Hex_Image is new Ada.Formatting.Integer_Image (
			Integer,
			Form => Ada.Formatting.Simple,
			Zero_Sign => Ada.Formatting.None,
			Plus_Sign => Ada.Formatting.None,
			Base => 16,
			Casing => Ada.Formatting.Upper,
			Width => 4,
			Padding => '0');
	begin
		pragma Assert (Image (123) = " 123");
		pragma Assert (Trimed_Image (123) = "123");
		pragma Assert (Hex_Image (123) = "007B");
		null;
	end;
	declare
		type T is digits 5;
		function Image is new Ada.Formatting.Float_Image (T);
		function Hex_Image is new Ada.Formatting.Float_Image (
			T,
			Base => 16,
			Aft_Width => 2);
		function Simple_Hex_Image is new Ada.Formatting.Float_Image (
			T,
			Form => Ada.Formatting.Simple,
			Zero_Sign => Ada.Formatting.None,
			Plus_Sign => Ada.Formatting.None,
			Base => 16,
			Aft_Width => 2);
	begin
		pragma Assert (Image (1.25) = " 1.2500E+00");
		pragma Assert (Hex_Image (-1.25) = "-16#1.40#E+00");
		pragma Assert (Simple_Hex_Image (1.25) = "1.40E+00");
		null;
	end;
	declare
		type T is delta 0.001 range 0.000 .. 999.999;
		function Image is new Ada.Formatting.Fixed_Image (T);
		function Hex_Image is new Ada.Formatting.Fixed_Image (
			T,
			Base => 16,
			Aft_Width => 2);
	begin
		pragma Assert (Image (1.25) = " 1.250");
		pragma Assert (Hex_Image (1.25) = " 16#1.40#");
		null;
	end;
	declare
		type T is delta 0.001 digits 6;
		function Image is new Ada.Formatting.Decimal_Image (T);
	begin
		pragma Assert (Image (1.25) = " 1.250");
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end format;
