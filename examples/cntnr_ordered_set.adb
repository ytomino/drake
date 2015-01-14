with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Sets.Debug;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Limited_Ordered_Sets;
with Ada.Streams.Unbounded_Storage_IO;
-- with Ada.Text_IO;
procedure cntnr_Ordered_Set is
	use type Ada.Containers.Count_Type;
	package Sets is new Ada.Containers.Ordered_Sets (Integer);
	package ISets is new Ada.Containers.Indefinite_Ordered_Sets (Integer);
	package LSets is new Ada.Containers.Limited_Ordered_Sets (Integer);
	package Sets_Debug is new Sets.Debug;
	procedure Test_01 is
		X : Sets.Set;
	begin
		Sets.Insert (X, 2);
		pragma Assert (X.Length = 1);
		pragma Assert (Sets.Element (X.First) = 2);
		pragma Assert (Sets.Element (X.Last) = 2);
		Sets.Insert (X, 3);
		pragma Assert (X.Length = 2);
		pragma Assert (Sets.Element (X.First) = 2);
		pragma Assert (Sets.Element (X.Last) = 3);
		Sets.Insert (X, 1);
		pragma Assert (X.Length = 3);
		pragma Assert (Sets.Element (X.First) = 1);
		pragma Assert (Sets.Element (X.Last) = 3);
	end Test_01;
	pragma Debug (Test_01);
	procedure Test_02 is
		use type Sets.Cursor;
		X : Sets.Set;
	begin
		for I in 1 .. 10 loop
			Sets.Include (X, I);
		end loop;
		for I in 1 .. 10 loop
			Sets.Exclude (X, I);
		end loop;
		pragma Assert (X.Length = 0);
		pragma Assert (X.First = Sets.No_Element);
		pragma Assert (X.Last = Sets.No_Element);
		for I in 1 .. 10 loop
			Sets.Include (X, I);
		end loop;
		for I in reverse 1 .. 10 loop
			Sets.Exclude (X, I);
		end loop;
		pragma Assert (X.Length = 0);
		pragma Assert (X.First = Sets.No_Element);
		pragma Assert (X.Last = Sets.No_Element);
		for I in reverse 1 .. 10 loop
			Sets.Include (X, I);
		end loop;
		for I in 1 .. 10 loop
			Sets.Exclude (X, I);
		end loop;
		pragma Assert (X.Length = 0);
		pragma Assert (X.First = Sets.No_Element);
		pragma Assert (X.Last = Sets.No_Element);
		for I in reverse 1 .. 10 loop
			Sets.Include (X, I);
		end loop;
		for I in reverse 1 .. 10 loop
			Sets.Exclude (X, I);
		end loop;
		pragma Assert (X.Length = 0);
		pragma Assert (X.First = Sets.No_Element);
		pragma Assert (X.Last = Sets.No_Element);
	end Test_02;
	pragma Debug (Test_02);
	procedure Test_03 is
		use type Sets.Cursor;
		X : Sets.Set;
	begin
		Sets.Include (X, 5);
		Sets.Include (X, 1);
		Sets.Include (X, 7);
		Sets.Include (X, 3);
		Sets.Include (X, 9);
		pragma Assert (Sets.Element (X.Ceiling (0)) = 1);
		pragma Assert (Sets.Element (X.Ceiling (2)) = 3);
		pragma Assert (Sets.Element (X.Ceiling (4)) = 5);
		pragma Assert (Sets.Element (X.Ceiling (6)) = 7);
		pragma Assert (Sets.Element (X.Ceiling (8)) = 9);
		pragma Assert (X.Ceiling (10) = Sets.No_Element);
		pragma Assert (X.Floor (0) = Sets.No_Element);
		pragma Assert (Sets.Element (X.Floor (2)) = 1);
		pragma Assert (Sets.Element (X.Floor (4)) = 3);
		pragma Assert (Sets.Element (X.Floor (6)) = 5);
		pragma Assert (Sets.Element (X.Floor (8)) = 7);
		pragma Assert (Sets.Element (X.Floor (10)) = 9);
	end Test_03;
	pragma Debug (Test_03);
	procedure Test_04 is
		use type Sets.Cursor;
		X : aliased Sets.Set;
		I : Sets.Cursor;
		N : Integer;
	begin
		Sets.Include (X, 5);
		Sets.Include (X, 9);
		Sets.Include (X, 1);
		Sets.Include (X, 7);
		Sets.Include (X, 3);
		I := X.First;
		N := 1;
		while Sets.Has_Element (I) loop
			pragma Assert (X.Constant_Reference (I).Element.all = N);
			Sets.Next (I);
			N := N + 2;
		end loop;
		I := X.Last;
		N := 9;
		while Sets.Has_Element (I) loop
			pragma Assert (X.Constant_Reference (I).Element.all = N);
			Sets.Previous (I);
			N := N - 2;
		end loop;
	end Test_04;
	pragma Debug (Test_04);
	procedure Test_05 is
		use type Sets.Set;
		X, Y : Sets.Set;
	begin
		Sets.Insert (X, 100);
		Sets.Insert (X, 200);
		Y := X;
		pragma Assert (X = Y);
		pragma Assert (Sets.Equivalent_Sets (X, Y));
		Sets.Insert (X, 300);
		pragma Assert (X /= Y);
		pragma Assert (not Sets.Equivalent_Sets (X, Y));
		Sets.Insert (Y, 300);
		pragma Assert (X = Y);
		pragma Assert (Sets.Equivalent_Sets (X, Y));
	end Test_05;
	pragma Debug (Test_05);
	procedure Test_06 is
		X, Y : Sets.Set;
	begin
		Sets.Insert (X, 100);
		Sets.Insert (X, 200);
		Sets.Insert (Y, 100);
		Sets.Insert (Y, 200);
		pragma Assert (X.Overlap (Y));
		pragma Assert (Y.Overlap (X));
		pragma Assert (X.Is_Subset (Y));
		pragma Assert (Y.Is_Subset (X));
		Sets.Delete (X, 100);
		pragma Assert (X.Overlap (Y));
		pragma Assert (Y.Overlap (X));
		pragma Assert (X.Is_Subset (Y));
		pragma Assert (not Y.Is_Subset (X));
		Sets.Delete (X, 200);
		pragma Assert (not X.Overlap (Y));
		pragma Assert (not Y.Overlap (X));
		pragma Assert (X.Is_Subset (Y));
		pragma Assert (not Y.Is_Subset (X));
		Sets.Insert (X, 300);
		pragma Assert (not X.Overlap (Y));
		pragma Assert (not Y.Overlap (X));
		pragma Assert (not X.Is_Subset (Y));
		pragma Assert (not Y.Is_Subset (X));
	end Test_06;
	pragma Debug (Test_06);
	procedure Test_07 is
		type Integer_Array is array (Positive range <>) of Integer;
		function To_Set is new Sets.Generic_Array_To_Set (Positive, Integer_Array);
		use type Sets.Set;
		X, Y, Z : Sets.Set;
	begin
		X := To_Set ((1, 2, 3));
		Y := To_Set ((1, 3, 5));
		pragma Assert ((X and Y) = To_Set ((1, 3)));
		pragma Assert ((X or Y) = To_Set ((1, 2, 3, 5)));
		pragma Assert ((X xor Y) = To_Set ((2, 5)));
		pragma Assert ((X - Y) = Sets.To_Set (2));
		Z := X;
		Sets.Intersection (Z, Y);
		pragma Assert (Z = To_Set ((1, 3)));
		Z := X;
		Sets.Union (Z, Y);
		pragma Assert (Z = To_Set ((1, 2, 3, 5)));
		Z := X;
		Sets.Symmetric_Difference (Z, Y);
		pragma Assert (Z = To_Set ((2, 5)));
		Z := X;
		Sets.Difference (Z, Y);
		pragma Assert (Z = Sets.To_Set (2));
	end Test_07;
	pragma Debug (Test_07);
begin
	declare
		X : Sets.Set;
	begin
		for I in 1 .. 5 loop
			Sets.Insert (X, I);
		end loop;
		Sets_Debug.Dump (X, Message => Ada.Debug.Source_Location);
		for I in reverse 6 .. 10 loop
			Sets.Insert (X, I);
		end loop;
		Sets_Debug.Dump (X, Message => Ada.Debug.Source_Location);
		for I in 11 .. 15 loop
			Sets.Insert (X, I);
		end loop;
		Sets_Debug.Dump (X, Message => Ada.Debug.Source_Location);
		for I in reverse 16 .. 20 loop
			Sets.Insert (X, I);
		end loop;
		Sets_Debug.Dump (X, Message => Ada.Debug.Source_Location);
		for I in 6 .. 15 loop
			Sets.Delete (X, I);
		end loop;
		Sets_Debug.Dump (X, Message => Ada.Debug.Source_Location);
		for I in 1 .. 5 loop
			Sets.Delete (X, I);
		end loop;
		for I in 16 .. 20 loop
			Sets.Delete (X, I);
		end loop;
		Sets_Debug.Dump (X, Message => Ada.Debug.Source_Location);
		pragma Assert (X.Is_Empty);
	end;
	Stream_Test : declare
		package USIO renames Ada.Streams.Unbounded_Storage_IO;
		X : Sets.Set;
		IX : ISets.Set;
		Buffer : USIO.Buffer_Type;
	begin
		-- Definite -> Inefinite (0)
		Sets.Set'Write (USIO.Stream (Buffer), X); -- write empty
		ISets.Insert (IX, 9);
		pragma Assert (IX.Length = 1);
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		ISets.Set'Read (USIO.Stream (Buffer), IX);
		pragma Assert (IX.Length = 0);
		-- Indefinite -> Definite (1)
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		ISets.Insert (IX, 10);
		pragma Assert (IX.Length = 1);
		ISets.Set'Write (USIO.Stream (Buffer), IX); -- write 'b'
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		Sets.Set'Read (USIO.Stream (Buffer), X);
		pragma Assert (X.Length = 1);
		pragma Assert (Sets.Element (X.First) = 10);
	end Stream_Test;
	pragma Debug (Ada.Debug.Put ("OK"));
end cntnr_Ordered_Set;
