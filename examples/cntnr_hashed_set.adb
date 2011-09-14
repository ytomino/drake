with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Limited_Hashed_Sets;
with Ada.Streams.Buffer_Storage_IO;
-- with Ada.Text_IO;
procedure cntnr_Hashed_Set is
	use type Ada.Containers.Count_Type;
	function Hash (X : Integer) return Ada.Containers.Hash_Type is
	begin
		return Ada.Containers.Hash_Type (X rem 10);
	end Hash;
	package Sets is new Ada.Containers.Hashed_Sets (
		Integer,
		Hash => Hash,
		Equivalent_Elements => "=");
	package ISets is new Ada.Containers.Indefinite_Hashed_Sets (
		Integer,
		Hash => Hash,
		Equivalent_Elements => "=");
	package LSets is new Ada.Containers.Limited_Hashed_Sets (
		Integer,
		Hash => Hash,
		Equivalent_Elements => "=");
--	procedure Dump (X : Sets.Set) is
--		I : Sets.Cursor := X.First;
--	begin
--		while Sets.Has_Element (I) loop
--			Ada.Text_IO.Put (Sets.Element(I)'Img);
--			Sets.Next (I);
--		end loop;
--		Ada.Text_IO.New_Line;
--	end Dump;
	procedure Test_01 is
		X : Sets.Set;
	begin
		Sets.Insert (X, 2);
		pragma Assert (X.Length = 1);
		Sets.Insert (X, 3);
		pragma Assert (X.Length = 2);
		Sets.Insert (X, 1);
		pragma Assert (X.Length = 3);
		pragma Assert (Sets.Element (X.Find (3)) = 3);
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
		for I in 1 .. 10 loop
			Sets.Include (X, I);
		end loop;
		for I in reverse 1 .. 10 loop
			Sets.Exclude (X, I);
		end loop;
		pragma Assert (X.Length = 0);
		pragma Assert (X.First = Sets.No_Element);
		for I in reverse 1 .. 10 loop
			Sets.Include (X, I);
		end loop;
		for I in 1 .. 10 loop
			Sets.Exclude (X, I);
		end loop;
		pragma Assert (X.Length = 0);
		pragma Assert (X.First = Sets.No_Element);
		for I in reverse 1 .. 10 loop
			Sets.Include (X, I);
		end loop;
		for I in reverse 1 .. 10 loop
			Sets.Exclude (X, I);
		end loop;
		pragma Assert (X.Length = 0);
		pragma Assert (X.First = Sets.No_Element);
	end Test_02;
	pragma Debug (Test_02);
	procedure Test_03 is
		use type Sets.Cursor;
		X : aliased Sets.Set;
		I : Sets.Cursor;
		type CA is array (1 .. 9) of Boolean;
		Check : CA := (others => False);
	begin
		Sets.Include (X, 5);
		Sets.Include (X, 9);
		Sets.Include (X, 1);
		Sets.Include (X, 7);
		Sets.Include (X, 3);
		I := X.First;
		while Sets.Has_Element (I) loop
			Check (X.Constant_Reference (I).Element.all) := True;
			Sets.Next (I);
		end loop;
		pragma Assert (Check = CA'(1 | 3 | 5 | 7 | 9 => True, others => False));
	end Test_03;
	pragma Debug (Test_03);
	procedure Test_04 is
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
	end Test_04;
	pragma Debug (Test_04);
	procedure Test_05 is
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
	end Test_05;
	pragma Debug (Test_05);
	procedure Test_06 is
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
	end Test_06;
	pragma Debug (Test_06);
begin
	Stream_Test : declare
		X : Sets.Set;
		IX : ISets.Set;
		Buffer : Ada.Streams.Buffer_Storage_IO.Buffer;
	begin
		-- Definite -> Inefinite (0)
		Sets.Set'Write (Buffer.Stream, X); -- write empty
		ISets.Insert (IX, 9);
		pragma Assert (IX.Length = 1);
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (Buffer.Stream.all), 1);
		ISets.Set'Read (Buffer.Stream, IX);
		pragma Assert (IX.Length = 0);
		-- Indefinite -> Definite (1)
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (Buffer.Stream.all), 1);
		ISets.Insert (IX, 10);
		pragma Assert (IX.Length = 1);
		ISets.Set'Write (Buffer.Stream, IX); -- write 'b'
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (Buffer.Stream.all), 1);
		Sets.Set'Read (Buffer.Stream, X);
		pragma Assert (X.Length = 1);
		pragma Assert (Sets.Element (X.First) = 10);
	end Stream_Test;
	pragma Debug (Ada.Debug.Put ("OK"));
end cntnr_Hashed_Set;
