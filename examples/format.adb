with Ada.Formatting;
procedure format is
begin
	Integer : declare
		type T is range -999 .. 999;
		function Image is new Ada.Formatting.Integer_Image (T);
		function Trimed_Image is new Ada.Formatting.Integer_Image (
			T,
			Signs => Ada.Formatting.Triming_Sign_Marks);
		function Hex_Image is new Ada.Formatting.Integer_Image (
			T,
			Base => 16);
		function Simple_Hex_Image is
			new Ada.Formatting.Integer_Image (
				T,
				Form => Ada.Formatting.Simple,
				Signs => Ada.Formatting.Triming_Sign_Marks,
				Base => 16,
				Digits_Width => 4);
	begin
		pragma Assert (Image (123) = " 123");
		pragma Assert (Trimed_Image (123) = "123");
		pragma Assert (Hex_Image (123) = " 16#7B#");
		pragma Assert (Simple_Hex_Image (123) = "007B");
		null;
	end Integer;
	Modular : declare
		type T is mod 1024;
		function Image is new Ada.Formatting.Modular_Image (T);
		function Hex_Image is new Ada.Formatting.Modular_Image (
			T,
			Base => 16);
		function Simple_Hex_Image is
			new Ada.Formatting.Modular_Image (
				T,
				Form => Ada.Formatting.Simple,
				Signs => Ada.Formatting.Triming_Unsign_Marks,
				Base => 16,
				Digits_Width => 4);
	begin
		pragma Assert (Image (123) = " 123");
		pragma Assert (Hex_Image (123) = " 16#7B#");
		pragma Assert (Simple_Hex_Image (123) = "007B");
		null;
	end Modular;
	Float : declare
		type T is digits 5;
		function Image is new Ada.Formatting.Float_Image (T);
		function Hex_Image is new Ada.Formatting.Float_Image (
			T,
			Base => 16,
			Aft_Width => 2);
		function Simple_Hex_Image is new Ada.Formatting.Float_Image (
			T,
			Form => Ada.Formatting.Simple,
			Signs => Ada.Formatting.Triming_Sign_Marks,
			Base => 16,
			Aft_Width => 2);
	begin
		pragma Assert (Image (1.25) = " 1.2500E+00");
		pragma Assert (Hex_Image (-1.25) = "-16#1.40#E+00");
		pragma Assert (Simple_Hex_Image (1.25) = "1.40E+00");
		null;
	end Float;
	Fixed : declare
		type T is delta 0.001 range 0.000 .. 999.999;
		function Bin_Image is new Ada.Formatting.Fixed_Image (
			T,
			Base => 2,
			Aft_Width => 3);
		function Image is new Ada.Formatting.Fixed_Image (T);
		function Hex_Image is new Ada.Formatting.Fixed_Image (
			T,
			Base => 16,
			Aft_Width => 2);
	begin
		pragma Assert (Bin_Image (1.25) = " 2#1.010#");
		pragma Assert (Image (1.25) = " 1.250");
		pragma Assert (Hex_Image (1.25) = " 16#1.40#");
		null;
	end Fixed;
	Decimal : declare
		type T is delta 0.001 digits 6;
		function Image is new Ada.Formatting.Decimal_Image (T);
	begin
		pragma Assert (Image (1.25) = " 1.250");
		pragma Assert (Image (-1.25) = "-1.250");
		null;
	end Decimal;
	pragma Debug (Ada.Debug.Put ("OK"));
end format;
