with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Limited_Doubly_Linked_Lists;
with Ada.Text_IO;
procedure cntnr_d_Linked_List is
	use type Ada.Containers.Count_Type;
	package Lists is new Ada.Containers.Doubly_Linked_Lists (Character);
	procedure Dump (X : Lists.List) is
		I : Lists.Cursor := X.First;
	begin
		while Lists.Has_Element (I) loop
			Ada.Text_IO.Put (Lists.Element (I)'Img);
			Ada.Text_IO.Put (", ");
			Lists.Next (I);
		end loop;
		Ada.Text_IO.New_Line;
	end Dump;
	procedure Test_01 is
		X : Lists.List;
	begin
		Lists.Append (X, 'A');
		pragma Assert (X.Length = 1);
		pragma Assert (Lists.Element (X.First) = 'A');
		pragma Assert (Lists.Element (X.Last) = 'A');
		Lists.Append (X, 'B');
		pragma Assert (X.Length = 2);
		pragma Assert (Lists.Element (X.First) = 'A');
		pragma Assert (Lists.Element (X.Last) = 'B');
		Lists.Append (X, 'C');
		pragma Assert (X.Length = 3);
		pragma Assert (Lists.Element (X.First) = 'A');
		pragma Assert (Lists.Element (X.Last) = 'C');
	end Test_01;
	pragma Debug (Test_01);
	procedure Test_02 is
		use type Lists.Cursor;
		X : Lists.List;
		C : Character;
	begin
		for I in Character'('A') .. 'F' loop
			Lists.Append (X, I);
		end loop;
		C := 'A';
		declare
			I : Lists.Cursor := X.First;
			Last : Lists.Cursor := X.Last;
		begin
			while I <= Last loop
				pragma Assert (Lists.Element (I) = C);
				C := Character'Succ (C);
				Lists.Next (I);
			end loop;
		end;
		declare
			I : Lists.Cursor := X.Last;
			First : Lists.Cursor := X.First;
		begin
			while I >= First loop
				C := Character'Pred (C);
				pragma Assert (Lists.Element (I) = C);
				Lists.Previous (I);
			end loop;
		end;
		declare
			I : Lists.Cursor := X.First;
			Last : Lists.Cursor := X.Last;
		begin
			if I <= Last then
				loop
					pragma Assert (Lists.Element (I) = C);
					C := Character'Succ (C);
					exit when I = Last;
					Lists.Next (I);
				end loop;
			end if;
		end;
		declare
			I : Lists.Cursor := X.Last;
			First : Lists.Cursor := X.First;
		begin
			if I >= First then
				loop
					C := Character'Pred (C);
					pragma Assert (Lists.Element (I) = C);
					exit when I = First;
					Lists.Previous (I);
				end loop;
			end if;
		end;
		declare
			I : Lists.Cursor := X.First;
			Last : Lists.Cursor := Lists.Previous (X.Last);
		begin
			if I <= Last then
				loop
					pragma Assert (Lists.Element (I) = C);
					exit when I = Last;
					C := Character'Succ (C);
					Lists.Next (I);
				end loop;
			end if;
		end;
		pragma Assert (C = 'E');
		Lists.Clear (X);
		declare
			I : Lists.Cursor := X.First;
			Last : Lists.Cursor := X.Last;
		begin
			while I <= Last loop
				pragma Assert (False);
				Lists.Next (I);
			end loop;
		end;
		declare
			I : Lists.Cursor := X.Last;
			First : Lists.Cursor := X.First;
		begin
			while I >= First loop
				pragma Assert (False);
				Lists.Previous (I);
			end loop;
		end;
	end Test_02;
	pragma Debug (Test_02);
	procedure Test_03 is
		X : Lists.List;
		I : Lists.Cursor;
	begin
		Lists.Append (X, 'A');
		Lists.Append (X, 'B');
		Lists.Append (X, 'C');
		Lists.Reverse_Elements (X);
		pragma Assert (X.Length = 3);
		pragma Assert (X.First_Element = 'C');
		pragma Assert (Lists.Element (Lists.Next (X.First)) = 'B');
		pragma Assert (X.Last_Element = 'A');
		Lists.Swap_Links (X, X.First, X.Last);
		declare
			Data : String := "ABC";
			Index : Integer := 1;
		begin
			I := X.First;
			while Lists.Has_Element (I) loop
				pragma Assert (Lists.Element (I) = Data (Index));
				Index := Integer'Succ (Index);
				Lists.Next (I);
			end loop;
			I := X.Last;
			while Lists.Has_Element (I) loop
				Index := Integer'Pred (Index);
				pragma Assert (Lists.Element (I) = Data (Index));
				Lists.Previous (I);
			end loop;
		end;
		Lists.Swap_Links (X, X.First, Lists.Next (X.First));
		declare
			Data : String := "BAC";
			Index : Integer := 1;
		begin
			I := X.First;
			while Lists.Has_Element (I) loop
				pragma Assert (Lists.Element (I) = Data (Index));
				Index := Integer'Succ (Index);
				Lists.Next (I);
			end loop;
			I := X.Last;
			while Lists.Has_Element (I) loop
				Index := Integer'Pred (Index);
				pragma Assert (Lists.Element (I) = Data (Index));
				Lists.Previous (I);
			end loop;
		end;
	end Test_03;
	pragma Debug (Test_03);
	procedure Test_04 is
		use type Lists.Cursor;
		X : Lists.List;
		Y : Lists.List;
	begin
		Lists.Append (X, 'A');
		Lists.Append (X, 'B');
		Lists.Append (Y, '1');
		Lists.Append (Y, '2');
		Lists.Splice (X, Lists.No_Element, Y);
		pragma Assert (X.Length = 4);
		pragma Assert (X.First_Element = 'A');
		pragma Assert (X.Last_Element = '2');
		pragma Assert (Y.Length = 0);
		pragma Assert (Y.First = Lists.No_Element);
		pragma Assert (Y.Last = Lists.No_Element);
	end Test_04;
	pragma Debug (Test_04);
	procedure Test_05 is
		use type Lists.List;
		package Sorting is new Lists.Generic_Sorting;
		X : Lists.List;
		Y : Lists.List;
	begin
		Lists.Append (X, '1');
		Lists.Append (X, '3');
		Lists.Append (X, '5');
		Lists.Append (Y, '2');
		Lists.Append (Y, '4');
		Lists.Append (Y, '6');
		Sorting.Merge (X, Y);
		pragma Assert (X.Length = 6);
		pragma Assert (Y.Length = 0);
		pragma Assert (Sorting.Is_Sorted (X));
		pragma Assert (Lists.Is_Empty (Y));
		Lists.Append (Y, '4');
		Lists.Append (Y, '2');
		Lists.Append (Y, '5');
		Lists.Append (Y, '3');
		Lists.Append (Y, '1');
		Lists.Append (Y, '6');
		Sorting.Sort (Y);
		pragma Assert (Sorting.Is_Sorted (Y));
		pragma Assert (X = Y);
	end Test_05;
	pragma Debug (Test_05);
	procedure Test_06 is
		use type Lists.Cursor;
		X : Lists.List;
		Y : Lists.List;
		X_F : Lists.Cursor;
		Y_F : Lists.Cursor;
	begin
		Lists.Append (X, 'a');
		X_F := X.First;
		Y := X; -- shared
		Y_F := Y.First; -- read operation for Y
		pragma Assert (X.First = X_F, "should keep X");
		pragma Assert (Y.First = Y_F, "should keep Y");
		pragma Assert (X_F /= Y_F, "should be copied");
		Y := X; -- shared
		Y_F := X.First; -- read operation for X, result is garbage
		Y_F := Y.First;
		pragma Assert (X.First = X_F, "should keep X");
		pragma Assert (Y.First = Y_F, "should keep Y");
		pragma Assert (X_F /= Y_F, "should be copied");
		Y := X; -- shared
		Lists.Append (Y, 'b'); -- write operation for Y
		Y_F := Y.First;
		pragma Assert (X.First = X_F, "should keep X");
		pragma Assert (Y.First = Y_F, "should keep Y");
		pragma Assert (X_F /= Y_F, "should be copied");
		Y := X; -- shared
		Lists.Append (X, 'b'); -- write operation for X
		Y_F := Y.First;
		pragma Assert (X.First = X_F, "should keep X");
		pragma Assert (Y.First = Y_F, "should keep Y");
		pragma Assert (X_F /= Y_F, "should be copied");
	end Test_06;
	pragma Debug (Test_06);
	pragma Debug (Ada.Debug.Put ("OK"));
begin
	null;
end cntnr_d_Linked_List;
