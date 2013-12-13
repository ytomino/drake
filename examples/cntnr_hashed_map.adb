with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Limited_Hashed_Maps;
with Ada.Streams.Unbounded_Storage_IO;
-- with Ada.Text_IO;
procedure cntnr_Hashed_Map is
	use type Ada.Containers.Count_Type;
	function Hash (X : Character) return Ada.Containers.Hash_Type is
	begin
		return Character'Pos (X);
	end Hash;
	package Maps is new Ada.Containers.Hashed_Maps (
		Character,
		Integer,
		Hash => Hash,
		Equivalent_Keys => "=");
	package IMaps is new Ada.Containers.Indefinite_Hashed_Maps (
		Character,
		Integer,
		Hash => Hash,
		Equivalent_Keys => "=");
	package LMaps is new Ada.Containers.Limited_Hashed_Maps (
		Character,
		Integer,
		Hash => Hash,
		Equivalent_Keys => "=");
--	procedure Dump (X : Maps.Map) is
--		I : Maps.Cursor := X.First;
--	begin
--		while Maps.Has_Element (I) loop
--			Ada.Text_IO.Put (Maps.Key(I)'Img);
--			Ada.Text_IO.Put (" =");
--			Ada.Text_IO.Put (Maps.Element(I)'Img);
--			Ada.Text_IO.Put (", ");
--			Maps.Next (I);
--		end loop;
--		Ada.Text_IO.New_Line;
--	end Dump;
	procedure Test_01 is
		X : Maps.Map;
	begin
		Maps.Insert (X, 'B', 15);
		pragma Assert (X.Length = 1);
		pragma Assert (X.Element ('B') = 15);
		Maps.Insert (X, 'A', 10);
		pragma Assert (X.Length = 2);
		pragma Assert (X.Element ('A') = 10);
		pragma Assert (X.Element ('B') = 15);
		Maps.Insert (X, 'C', 20);
		pragma Assert (X.Length = 3);
		pragma Assert (X.Element ('A') = 10);
		pragma Assert (X.Element ('B') = 15);
		pragma Assert (X.Element ('C') = 20);
	end Test_01;
	pragma Debug (Test_01);
	procedure Test_02 is
		use type Maps.Cursor;
		X : aliased Maps.Map;
	begin
		for I in Character'('A') .. 'F' loop
			Maps.Insert (X, I, Character'Pos (I));
		end loop;
		pragma Assert (X.Length = 6);
		declare
			I : Maps.Cursor := X.First;
			type CA is array (Character range 'A' .. 'F') of Boolean;
			Check : CA := (others => False);
		begin
			while Maps.Has_Element (I) loop
				Check (Maps.Key (I)) := True;
				Maps.Next (I);
			end loop;
			pragma Assert (Check = CA'(others => True));
		end;
		X.Delete ('C');
		pragma Assert (X.Length = 5);
		-- iteration
		declare
			Ite : Maps.Map_Iterator_Interfaces.Forward_Iterator'Class := X.Iterate;
			Pos : Maps.Cursor := Maps.Map_Iterator_Interfaces.First (Ite);
			type CA is array (Character range 'A' .. 'F') of Boolean;
			Check : CA := (others => False);
		begin
			while Pos /= Maps.No_Element loop
				Check (Maps.Key (Pos)) := True;
				Pos := Maps.Map_Iterator_Interfaces.Next (Ite, Pos);
			end loop;
			pragma Assert (Check = CA'('C' => False, others => True));
		end;
	end Test_02;
	pragma Debug (Test_02);
begin
	Stream_Test : declare
		package USIO renames Ada.Streams.Unbounded_Storage_IO;
		X : Maps.Map;
		IX : IMaps.Map;
		Buffer : USIO.Buffer_Type;
	begin
		-- Definite -> Inefinite (0)
		Maps.Map'Write (USIO.Stream (Buffer), X); -- write empty
		IMaps.Insert (IX, '#', 9);
		pragma Assert (IX.Length = 1);
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		IMaps.Map'Read (USIO.Stream (Buffer), IX);
		pragma Assert (IX.Length = 0);
		-- Indefinite -> Definite (1)
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		IMaps.Insert (IX, '$', 10);
		pragma Assert (IX.Length = 1);
		IMaps.Map'Write (USIO.Stream (Buffer), IX); -- write 'b'
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		Maps.Map'Read (USIO.Stream (Buffer), X);
		pragma Assert (X.Length = 1);
		pragma Assert (Maps.Element (X.First) = 10);
		pragma Assert (X.Element ('$') = 10);
	end Stream_Test;
	pragma Debug (Ada.Debug.Put ("OK"));
end cntnr_Hashed_Map;
