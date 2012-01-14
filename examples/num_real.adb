-- This file is UTF-8.
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Real_Arrays;
-- with Ada.Text_IO;
procedure num_real is
--	procedure Dump (A : Ada.Numerics.Real_Arrays.Real_Matrix) is
--	begin
--		for I in A'Range (1) loop
--			for J in A'Range (2) loop
--				Ada.Text_IO.Put (A (I, J)'Img);
--				null;
--			end loop;
--		end loop;
--		Ada.Text_IO.New_Line;
--	end Dump;
	procedure Test_Pi is
		X : Long_Long_Float := Ada.Numerics.π; -- use "π"
	begin
		pragma Assert (X = Ada.Numerics.Pi);
		null;
	end Test_Pi;
	pragma Debug (Test_Pi);
	procedure Test_Sin is
		use Ada.Numerics.Elementary_Functions;
	begin
		pragma Assert (Sin (0.0) = 0.0);
		pragma Assert (Sin (Ada.Numerics.Pi / 2.0) = 1.0);
		pragma Assert (abs (Sin (Ada.Numerics.Pi) - 0.0) < 1.0e-5);
		pragma Assert (abs (Sin (2.0 * Ada.Numerics.Pi) - 0.0) < 1.0e-5);
		null;
	end Test_Sin;
	pragma Debug (Test_Sin);
	procedure Test_Arctan is
		use Ada.Numerics.Elementary_Functions;
	begin
		pragma Assert (Arctan (0.0, 1.0) = 0.0);
		pragma Assert (Arctan (1.0, 0.0) = Ada.Numerics.Pi / 2.0);
		pragma Assert (abs (Arctan (0.0, -1.0) - Ada.Numerics.Pi) < 1.0e-6); -- precision problem?
		pragma Assert (Arctan (-1.0, 0.0) = - Ada.Numerics.Pi / 2.0);
		null;
	end Test_Arctan;
	pragma Debug (Test_Arctan);
	procedure Test_Power is
		use Ada.Numerics.Elementary_Functions;
	begin
		pragma Assert (10.0 ** 2.0 = 100.0);
		pragma Assert (2.0 ** 3.0 = 8.0);
		null;
	end Test_Power;
	pragma Debug (Test_Power);
	procedure Test_Real_Arrays is
		use Ada.Numerics.Real_Arrays;
		Data1 : Real_Vector := (3.0, 4.0);
		Data2 : Real_Vector := (-1.0, 1.0);
		DataM : Real_Matrix := ((-1.0, 1.0), (2.0, -2.0));
		M : Real_Matrix (1 .. 2, 1 .. 2);
		Data33 : Real_Matrix := (
			(0.0, -2.0, 0.0),
			(-1.0, 3.0, 1.0),
			(4.0, 2.0, 1.0));
		DataR : Real_Matrix := ((2.0, 3.0), (5.0, 6.0));
	begin
		pragma Assert (abs Data1 = 5.0);
		pragma Assert (abs Data2 = (1.0, 1.0));
		pragma Assert (- Data1 = (-3.0, -4.0));
		pragma Assert (Data1 + Data2 = (2.0, 5.0));
		pragma Assert (Data1 - Data2 = (4.0, 3.0));
		pragma Assert (Data1 * 2.0 = (6.0, 8.0));
		M := Data1 * Data2;
		--Ada.Text_IO.Put_Line (M (1, 1)'Img & M (1, 2)'Img);
		--Ada.Text_IO.Put_Line (M (2, 1)'Img & M (2, 2)'Img);
		pragma Assert (M = (
			(-3.0, 3.0),
			(-4.0, 4.0)));
		pragma Assert (DataM * 2.0 = (
			(-2.0, 2.0),
			(4.0, -4.0)));
		pragma Assert (abs DataM = (
			(1.0, 1.0),
			(2.0, 2.0)));
		pragma Assert (- DataM = (
			(1.0, -1.0),
			(-2.0, 2.0)));
		pragma Assert (Determinant (Data33) = -10.0);
		pragma Assert (Unit_Matrix (2) = (
			(1.0, 0.0),
			(0.0, 1.0)));
		M := Inverse (DataR) * DataR;
		pragma Assert (
			M (1, 1) = 1.0 and
			M (1, 2) = 0.0 and
			abs M (2, 1) < 1.0e-5 and
			abs (M (2, 2) - 1.0) < 1.0e-5);
	end Test_Real_Arrays;
	pragma Debug (Test_Real_Arrays);
	pragma Debug (Ada.Debug.Put ("OK"));
begin
	null;
end num_real;
