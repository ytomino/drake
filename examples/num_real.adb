-- This file is UTF-8.
pragma Wide_Character_Encoding (UTF8); -- for using π
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Long_Long_Elementary_Functions; -- linking check
procedure num_real is
	generic
		type T is digits <>;
	procedure Test;
	procedure Test is
		package EF is new Ada.Numerics.Generic_Elementary_Functions (T);
		package RA is new Ada.Numerics.Generic_Real_Arrays (T);
		use EF, RA;
	begin
		-- elementary functions
		pragma Assert (Sin (0.0) = 0.0);
		pragma Assert (Sin (Ada.Numerics.Pi / 2.0) = 1.0);
		pragma Assert (abs (Sin (Ada.Numerics.Pi) - 0.0) < 1.0e-5);
		pragma Assert (abs (Sin (2.0 * Ada.Numerics.Pi) - 0.0) < 1.0e-5);
		pragma Assert (Arctan (0.0, 1.0) = 0.0);
		pragma Assert (Arctan (1.0, 0.0) = Ada.Numerics.Pi / 2.0);
		pragma Assert (abs (Arctan (0.0, -1.0) - Ada.Numerics.Pi) < 1.0e-6); -- precision problem?
		pragma Assert (Arctan (-1.0, 0.0) = - Ada.Numerics.Pi / 2.0);
		pragma Assert (10.0 ** 2.0 = 100.0);
		pragma Assert (2.0 ** 3.0 = 8.0);
		-- arrays
		for I in 0 .. 9 loop
			declare
				X : Real_Vector (11 .. 10 + I);
				Y : Real_Vector (21 .. 20 + I);
			begin
				for J in X'Range loop
					X (J) := T (J);
				end loop;
				for J in Y'Range loop
					Y (J) := T (J);
				end loop;
				declare
					Z : Real_Vector := X + Y;
				begin
					pragma Assert (Z'First = X'First);
					pragma Assert (Z'Last = X'Last);
					for J in Z'Range loop
						pragma Assert (Z (J) = T (J) * 2.0 + 10.0);
						null;
					end loop;
				end;
			end;
		end loop;
		declare
			Data1 : Real_Vector := (3.0, 4.0);
			Data2 : Real_Vector := (-1.0, 1.0);
			DataM : Real_Matrix := ((-1.0, 1.0), (2.0, -2.0));
			V : Real_Vector (1 .. 2);
			M : Real_Matrix (1 .. 2, 1 .. 2);
			Data33 : Real_Matrix := (
				(0.0, -2.0, 0.0),
				(-1.0, 3.0, 1.0),
				(4.0, 2.0, 1.0));
			DataR : Real_Matrix := ((2.0, 3.0), (5.0, 6.0));
		begin
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
			pragma Assert (M (1, 1) = 1.0
				and then M (1, 2) = 0.0
				and then abs M (2, 1) < 1.0e-5
				and then abs (M (2, 2) - 1.0) < 1.0e-5);
			V := DataM * Data1;
			pragma Assert (V = (1.0, -2.0));
			V := Data1 * DataM;
			pragma Assert (V = (5.0, -5.0));
		end;
	end Test;
begin
	pragma Assert (Ada.Numerics.π = Ada.Numerics.Pi);
	declare
		procedure Short_Float_Test is new Test (Short_Float);
	begin
		Short_Float_Test;
	end;
	declare
		procedure Float_Test is new Test (Float);
	begin
		Float_Test;
	end;
	declare
		procedure Long_Float_Test is new Test (Long_Float);
	begin
		Long_Float_Test;
	end;
	declare
		procedure Long_Long_Float_Test is new Test (Long_Long_Float);
	begin
		Long_Long_Float_Test;
	end;
	declare
		type Custom_Float is digits 12;
		procedure Custom_Float_Test is new Test (Custom_Float);
	begin
		Custom_Float_Test;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end num_real;
