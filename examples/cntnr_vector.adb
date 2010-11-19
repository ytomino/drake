with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Limited_Vectors;
with Ada.Containers.Inside.Array_Sorting;
with Ada.Characters.ASCII.Handling;
with Ada.Text_IO;
procedure cntnr_Vector is
	function Custom_Eq (Left, Right : Character) return Boolean is
	begin
		return Ada.Characters.ASCII.Handling.To_Upper (Left) =
			Ada.Characters.ASCII.Handling.To_Upper (Right);
	end Custom_Eq;
	package Vectors is new Ada.Containers.Vectors (Positive, Character,
		"=" => Custom_Eq);
	package IVectors is new Ada.Containers.Indefinite_Vectors (Positive,
		Character, "=" => Custom_Eq);
	procedure Test_01 is
		type Data_Array is array (1 .. 10) of Integer;
		Data : Data_Array := (345, 63, 423, 504, 513, 804, 346, 98, 915, 30);
		function LT (Left, Right : Integer) return Boolean is
		begin
			return Data (Left) < Data (Right);
		end LT;
		procedure Swap (Left, Right : Integer) is
			Temp : Integer := Data (Left);
		begin
			Data (Left) := Data (Right);
			Data (Right) := Temp;
		end Swap;
	begin
		Ada.Containers.Inside.Array_Sorting.Insertion_Sort (
			Data'First, Data'Last, LT'Access, Swap'Access);
		pragma Assert (Ada.Containers.Inside.Array_Sorting.Is_Sorted (
			Data'First, Data'Last, LT'Access));
	end Test_01;
	pragma Debug (Test_01);
	procedure Test_02 is
		type Data_Array is array (1 .. 10) of Integer;
		Data : Data_Array := (345, 63, 423, 504, 513, 804, 346, 98, 915, 30);
		function LT (Left, Right : Integer) return Boolean is
		begin
			return Data (Left) < Data (Right);
		end LT;
		procedure Swap (Left, Right : Integer) is
			Temp : Integer := Data (Left);
		begin
			Data (Left) := Data (Right);
			Data (Right) := Temp;
			--for I in Data'Range loop
			--	Ada.Text_IO.Put (Integer'Image (Data (I)));
			--end loop;
			--Ada.Text_IO.New_Line;
		end Swap;
	begin
		Ada.Containers.Inside.Array_Sorting.In_Place_Merge_Sort (
			Data'First, Data'Last, LT'Access, Swap'Access);
		pragma Assert (Ada.Containers.Inside.Array_Sorting.Is_Sorted (
			Data'First, Data'Last, LT'Access));
	end Test_02;
	pragma Debug (Test_02);
	procedure Test_03 is
		function To_Vector is new Vectors.Generic_Array_To_Vector (String);
		use type Vectors.Vector;
		X : Vectors.Vector := To_Vector ("ABC");
		Y : Vectors.Vector := To_Vector ("abc");
	begin
		pragma Assert (X = Y);
		null;
	end Test_03;
	pragma Debug (Test_03);
	procedure Test_04 is
		use type Ada.Containers.Count_Type;
		use type Vectors.Vector;
		X : aliased Vectors.Vector := 'A' & 'B' & 'C';
		Y : aliased Vectors.Vector;
		Z : aliased Vectors.Vector;
	begin
		pragma Assert (X.Length = 3);
		pragma Assert (X.Last_Element = 'C');
		Y := X & 'D';
		Z := X & 'E';
		pragma Assert (Y.Length = 4);
		pragma Assert (Y.Last_Element = 'D');
		pragma Assert (Z.Length = 4);
		pragma Assert (Z.Last_Element = 'E');
		pragma Assert (X.Constant_Reference (1).Element =
			Y.Constant_Reference (1).Element);
		pragma Assert (X.Constant_Reference (1).Element /=
			Z.Constant_Reference (1).Element);
	end Test_04;
	pragma Debug (Test_04);
	procedure Test_05 is
		use type Ada.Containers.Count_Type;
		use type Vectors.Vector;
		X : aliased Vectors.Vector := 'A' & 'B' & 'C' & 'D';
	begin
		Vectors.Delete (X, 2, 2);
		pragma Assert (X.Length = 2);
		pragma Assert (X.First_Element = 'A');
		pragma Assert (X.Last_Element = 'D');
		Vectors.Insert (X, 2, 'Z');
		pragma Assert (X.Length = 3);
		pragma Assert (X.Element (1) = 'A');
		pragma Assert (X.Element (2) = 'Z');
		pragma Assert (X.Element (3) = 'D');
	end Test_05;
	pragma Debug (Test_05);
	procedure Test_06 is
		use type Ada.Containers.Count_Type;
		use type IVectors.Vector;
		X : aliased IVectors.Vector := 'A' & 'B' & 'C';
		Y : aliased IVectors.Vector;
		Z : aliased IVectors.Vector;
	begin
		pragma Assert (X.Length = 3);
		pragma Assert (X.Last_Element = 'C');
		Y := X & 'D';
		Z := X & 'E';
		pragma Assert (Y.Length = 4);
		pragma Assert (Y.Last_Element = 'D');
		pragma Assert (Z.Length = 4);
		pragma Assert (Z.Last_Element = 'E');
		pragma Assert (X.Constant_Reference (1).Element =
			Y.Constant_Reference (1).Element);
		pragma Assert (X.Constant_Reference (1).Element /=
			Z.Constant_Reference (1).Element);
		X := 'A' & 'B' & 'C' & 'D';
		IVectors.Delete (X, 2, 2);
		pragma Assert (X.Length = 2);
		pragma Assert (X.First_Element = 'A');
		pragma Assert (X.Last_Element = 'D');
		IVectors.Insert (X, 2, 'Z');
		pragma Assert (X.Length = 3);
		pragma Assert (X.Element (1) = 'A');
		pragma Assert (X.Element (2) = 'Z');
		pragma Assert (X.Element (3) = 'D');
	end Test_06;
	pragma Debug (Test_06);
	procedure Test_07 is
		Data : String := "asdfghjkl";
		function LT (Left, Right : Integer) return Boolean is
		begin
			return Data (Left) < Data (Right);
		end LT;
		procedure Swap (Left, Right : Integer) is
			Temp : Character := Data (Left);
		begin
			--Ada.Text_IO.Put (Data);
			--Ada.Text_IO.Put (" => ");
			Data (Left) := Data (Right);
			Data (Right) := Temp;
			--Ada.Text_IO.Put_Line (Data);
		end Swap;
	begin
		--Ada.Text_IO.Put_Line ("**** Test_07 ****");
		Ada.Containers.Inside.Array_Sorting.In_Place_Merge_Sort (
			Data'First, Data'Last, LT'Access, Swap'Access);
		pragma Assert (Ada.Containers.Inside.Array_Sorting.Is_Sorted (
			Data'First, Data'Last, LT'Access));
	end Test_07;
	pragma Debug (Test_07);
	procedure Test_08 is
		use Vectors;
		X : aliased Vectors.Vector;
	begin
		for I in Character'('A') .. 'Z' loop
			Append (X, I);
		end loop;
		ada.debug.put (string(X.Reference (1, 26).Element.all));
		pragma Assert (X.Reference.Element.all = "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		pragma Assert (X.Reference (1, 26).Element.all = "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		pragma Assert (X.Reference (2, 3).Element.all = "BC");
	end Test_08;
	pragma Debug (Test_08);
	pragma Debug (Ada.Debug.Put ("OK"));
begin
	null;
end cntnr_Vector;
