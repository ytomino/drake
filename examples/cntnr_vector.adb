with Ada.Characters.ASCII.Handling;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Limited_Vectors;
with Ada.Containers.Vectors;
with Ada.Streams.Unbounded_Storage_IO;
procedure cntnr_Vector is
	function Custom_Eq (Left, Right : Character) return Boolean is
	begin
		return Ada.Characters.ASCII.Handling.To_Upper (Left) =
			Ada.Characters.ASCII.Handling.To_Upper (Right);
	end Custom_Eq;
	function Custom_Le (Left, Right : Character) return Boolean is
	begin
		return Ada.Characters.ASCII.Handling.To_Upper (Left) <
			Ada.Characters.ASCII.Handling.To_Upper (Right);
	end Custom_Le;
	package Vectors is new Ada.Containers.Vectors (
		Positive,
		Character,
		"=" => Custom_Eq);
	package Vectors_Sorting is new Vectors.Generic_Sorting ("<" => Custom_Le);
	package IVectors is new Ada.Containers.Indefinite_Vectors (
		Positive,
		Character,
		"=" => Custom_Eq);
	package IVectors_Sorting is new IVectors.Generic_Sorting ("<" => Custom_Le);
	package LVectors is new Ada.Containers.Limited_Vectors (
		Positive,
		Character);
	procedure Test_03 is
		function To_Vector is new Vectors.Generic_Array_To_Vector (String);
		use type Vectors.Vector;
		X : aliased Vectors.Vector := To_Vector ("ABC");
		Y : aliased Vectors.Vector := To_Vector ("abc");
	begin
		pragma Assert (X = Y);
		pragma Assert (X.Length = 3);
		pragma Assert (X.Constant_Reference (X.First_Index).Element.all = 'A');
		X.Swap (1, 3);
		pragma Assert (String (X.Constant_Reference.Element.all) = "CBA");
		X := To_Vector ("zxcvbnm");
		Y := To_Vector ("ASDFGHJKL");
		Vectors_Sorting.Sort (X);
		Vectors_Sorting.Sort (Y);
		Vectors_Sorting.Merge (X, Y);
		pragma Assert (String (X.Constant_Reference.Element.all) = "AbcDFGHJKLmnSvxz");
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
		pragma Assert (X.Element (X.Last) = 'C');
		pragma Assert (X.Constant_Reference (X.Last_Index).Element.all = 'C');
		Y := X & 'D';
		Z := X & 'E';
		pragma Assert (Y.Length = 4);
		pragma Assert (Y.Element (Y.Last) = 'D');
		pragma Assert (Z.Length = 4);
		pragma Assert (Z.Element (Z.Last) = 'E');
		pragma Assert (X.Constant_Reference (1).Element /=
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
		pragma Assert (X.Element (X.First) = 'A');
		pragma Assert (X.Element (X.Last) = 'D');
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
		pragma Assert (X.Element (X.Last) = 'C');
		Y := X & 'D';
		Z := X & 'E';
		pragma Assert (Y.Length = 4);
		pragma Assert (Y.Element (Y.Last) = 'D');
		pragma Assert (Z.Length = 4);
		pragma Assert (Z.Element (Z.Last) = 'E');
		pragma Assert (X.Constant_Reference (1).Element /=
			Y.Constant_Reference (1).Element);
		pragma Assert (X.Constant_Reference (1).Element /=
			Z.Constant_Reference (1).Element);
		X := 'A' & 'B' & 'C' & 'D';
		IVectors.Delete (X, 2, 2);
		pragma Assert (X.Length = 2);
		pragma Assert (X.Element (X.First) = 'A');
		pragma Assert (X.Element (X.Last) = 'D');
		IVectors.Insert (X, 2, 'Z');
		pragma Assert (X.Length = 3);
		pragma Assert (X.Element (1) = 'A');
		pragma Assert (X.Element (2) = 'Z');
		pragma Assert (X.Element (3) = 'D');
	end Test_06;
	pragma Debug (Test_06);
	procedure Test_08 is
		use Vectors;
		X : aliased Vectors.Vector;
	begin
		for I in Character'('A') .. 'Z' loop
			Append (X, I);
		end loop;
		-- accessor
		-- ada.debug.put (string(X.Reference (1, 26).Element.all));
		pragma Assert (X.Reference.Element.all = "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		pragma Assert (X.Reference (1, 26).Element.all = "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		pragma Assert (X.Reference (2, 3).Element.all = "BC");
		-- forward iteration
		declare
			Ite : Vector_Iterator_Interfaces.Reversible_Iterator'Class := X.Iterate;
			Pos : Cursor := Vector_Iterator_Interfaces.First (Ite);
			C : Character := 'A';
		begin
			while Pos /= No_Element loop
				pragma Assert (X.Reference (Pos).Element.all = C);
				C := Character'Succ (C);
				Pos := Vector_Iterator_Interfaces.Next (Ite, Pos);
			end loop;
		end;
		-- forward iteration (Ada 2012)
		declare
			C : Character := 'A';
		begin
			for E of X loop
				pragma Assert (E = C);
				C := Character'Succ (C);
			end loop;
		end;
		-- reverse iteration
		declare
			Ite : Vector_Iterator_Interfaces.Reversible_Iterator'Class := X.Iterate;
			Pos : Cursor := Vector_Iterator_Interfaces.Last (Ite);
			C : Character := 'Z';
		begin
			while Pos /= No_Element loop
				pragma Assert (X.Reference (Pos).Element.all = C);
				C := Character'Pred (C);
				Pos := Vector_Iterator_Interfaces.Previous (Ite, Pos);
			end loop;
		end;
		-- reverse iteration (Ada 2012)
		declare
			C : Character := 'Z';
		begin
			for E of reverse X loop
				pragma Assert (E = C);
				C := Character'Pred (C);
			end loop;
		end;
	end Test_08;
	pragma Debug (Test_08);
begin
	Stream_Test : declare
		package USIO renames Ada.Streams.Unbounded_Storage_IO;
		X : Vectors.Vector;
		IX : IVectors.Vector;
		Buffer : USIO.Buffer_Type;
	begin
		-- Definite -> Inefinite (0)
		Vectors.Vector'Write (USIO.Stream (Buffer), X); -- write empty
		IVectors.Append (IX, 'a');
		pragma Assert (IX.Length = 1);
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		IVectors.Vector'Read (USIO.Stream (Buffer), IX);
		pragma Assert (IX.Length = 0);
		-- Indefinite -> Definite (1)
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		IVectors.Append (IX, 'b');
		pragma Assert (IX.Length = 1);
		IVectors.Vector'Write (USIO.Stream (Buffer), IX); -- write 'b'
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		Vectors.Vector'Read (USIO.Stream (Buffer), X);
		pragma Assert (X.Length = 1);
		pragma Assert (X.Element (1) = 'b');
	end Stream_Test;
	pragma Debug (Ada.Debug.Put ("OK"));
end cntnr_Vector;
