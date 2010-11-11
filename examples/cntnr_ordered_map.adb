with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Limited_Ordered_Maps;
with Ada.Text_IO;
procedure cntnr_Ordered_Map is
	use type Ada.Containers.Count_Type;
	package Maps is new Ada.Containers.Ordered_Maps (Character, Integer);
	procedure Dump (X : Maps.Map) is
		I : Maps.Cursor := X.First;
	begin
		while Maps.Has_Element (I) loop
			Ada.Text_IO.Put (Maps.Key(I)'Img);
			Ada.Text_IO.Put (" =");
			Ada.Text_IO.Put (Maps.Element(I)'Img);
			Ada.Text_IO.Put (", ");
			Maps.Next (I);
		end loop;
		Ada.Text_IO.New_Line;
	end Dump;
	procedure Test_01 is
		X : Maps.Map;
	begin
		Maps.Insert (X, 'B', 15);
		pragma Assert (X.Length = 1);
		pragma Assert (Maps.Key (X.First) = 'B');
		pragma Assert (Maps.Key (X.Last) = 'B');
		pragma Assert (Maps.Element (X.First) = 15);
		pragma Assert (Maps.Element (X.Last) = 15);
		Maps.Insert (X, 'A', 10);
		pragma Assert (X.Length = 2);
		pragma Assert (Maps.Key (X.First) = 'A');
		pragma Assert (Maps.Key (X.Last) = 'B');
		pragma Assert (Maps.Element (X.First) = 10);
		pragma Assert (Maps.Element (X.Last) = 15);
		Maps.Insert (X, 'C', 20);
		pragma Assert (X.Length = 3);
		pragma Assert (Maps.Key (X.First) = 'A');
		pragma Assert (Maps.Key (X.Last) = 'C');
		pragma Assert (Maps.Element (X.First) = 10);
		pragma Assert (Maps.Element (X.Last) = 20);
	end Test_01;
	pragma Debug (Test_01);
	procedure Test_02 is
		use type Maps.Cursor;
		X : Maps.Map;
		C : Character;
	begin
		for I in Character'('A') .. 'F' loop
			Maps.Insert (X, I, Character'Pos (I));
		end loop;
		C := 'A';
		declare
			I : Maps.Cursor := X.First;
			Last : Maps.Cursor := X.Last;
		begin
			while I <= Last loop
				pragma Assert (Maps.Key (I) = C);
				C := Character'Succ (C);
				Maps.Next (I);
			end loop;
		end;
		declare
			I : Maps.Cursor := X.Last;
			First : Maps.Cursor := X.First;
		begin
			while I >= First loop
				C := Character'Pred (C);
				pragma Assert (Maps.Key (I) = C);
				Maps.Previous (I);
			end loop;
		end;
		declare
			I : Maps.Cursor := X.First;
			Last : Maps.Cursor := X.Last;
		begin
			if I <= Last then
				loop
					pragma Assert (Maps.Key (I) = C);
					C := Character'Succ (C);
					exit when I = Last;
					Maps.Next (I);
				end loop;
			end if;
		end;
		declare
			I : Maps.Cursor := X.Last;
			First : Maps.Cursor := X.First;
		begin
			if I >= First then
				loop
					C := Character'Pred (C);
					pragma Assert (Maps.Key (I) = C);
					exit when I = First;
					Maps.Previous (I);
				end loop;
			end if;
		end;
		Maps.Clear (X);
		declare
			I : Maps.Cursor := X.First;
			Last : Maps.Cursor := X.Last;
		begin
			while I <= Last loop
				pragma Assert (False);
				Maps.Next (I);
			end loop;
		end;
		declare
			I : Maps.Cursor := X.Last;
			First : Maps.Cursor := X.First;
		begin
			while I >= First loop
				pragma Assert (False);
				Maps.Previous (I);
			end loop;
		end;
	end Test_02;
	pragma Debug (Test_02);
	pragma Debug (Ada.Debug.Put ("OK"));
begin
	null;
end cntnr_Ordered_Map;
