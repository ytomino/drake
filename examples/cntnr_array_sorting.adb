with Ada.Containers.Generic_Arrays;
with Ada.Unchecked_Deallocation;
procedure cntnr_array_sorting is
	type SA is access String;
	procedure Free is new Ada.Unchecked_Deallocation (String, SA);
	Data : SA := null;
	Comp_Count : Natural;
	Swap_Count : Natural;
	procedure Setup (Source : String) is
	begin
		Free (Data);
		Data := new String'(Source);
		Comp_Count := 0;
		Swap_Count := 0;
	end Setup;
	procedure Report (
		Message : String;
		Source_Location : String := Ada.Debug.Source_Location;
		Enclosing_Entity : String := Ada.Debug.Enclosing_Entity) is
	begin
		Ada.Debug.Put (
			Message & Natural'Image (Comp_Count) & Natural'Image (Swap_Count),
			Source_Location => Source_Location,
			Enclosing_Entity => Enclosing_Entity);
	end Report;
	procedure Increment (X : in out Natural) is
	begin
		X := X + 1;
	end Increment;
	function LT (Left, Right : Character) return Boolean is
		pragma Debug (Increment (Comp_Count));
	begin
		return Left < Right;
	end LT;
	procedure Swap (Data : in out SA; Left, Right : Integer) is
		pragma Debug (Increment (Swap_Count));
		Temp : Character := Data (Left);
	begin
		Data (Left) := Data (Right);
		Data (Right) := Temp;
	end Swap;
	package SA_Op is new Ada.Containers.Generic_Arrays (Positive, Character, String, SA);
	package SA_Reversing is new SA_Op.Generic_Reversing (Swap);
	package SA_Sorting is new SA_Op.Generic_Sorting (LT, Swap);
begin
	-- Is_Sorted
	begin
		Setup ("");
		pragma Assert (SA_Sorting.Is_Sorted (Data));
		Setup ("ABCDEF");
		pragma Assert (SA_Sorting.Is_Sorted (Data));
		Setup ("ABCCDD");
		pragma Assert (SA_Sorting.Is_Sorted (Data));
		Setup ("ABCBA");
		pragma Assert (not SA_Sorting.Is_Sorted (Data));
	end;
	-- In_Place_Reverse
	begin
		Setup ("");
		SA_Reversing.Reverse_Elements (Data);
		pragma Assert (Data.all = "");
		pragma Assert (Swap_Count = 0);
		Setup ("A");
		SA_Reversing.Reverse_Elements (Data);
		pragma Assert (Data.all = "A");
		pragma Assert (Swap_Count = 0);
		Setup ("BA");
		SA_Reversing.Reverse_Elements (Data);
		pragma Assert (Data.all = "AB");
		pragma Assert (Swap_Count = 1);
		Setup ("CBA");
		SA_Reversing.Reverse_Elements (Data);
		pragma Assert (Data.all = "ABC");
		pragma Assert (Swap_Count = 1);
		Setup ("DCBA");
		SA_Reversing.Reverse_Elements (Data);
		pragma Assert (Data.all = "ABCD");
		pragma Assert (Swap_Count = 2);
	end;
	-- rotate an empty data
	declare
		Source : constant String := "";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		SA_Reversing.Reverse_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "");
		pragma Assert (Swap_Count = 0);
		Setup (Source);
		SA_Reversing.Juggling_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "");
		pragma Assert (Swap_Count = 0);
	end;
	-- rotate 1
	declare
		Source : constant String := "A";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		SA_Reversing.Reverse_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "A");
		pragma Assert (Swap_Count = 0);
		Setup (Source);
		SA_Reversing.Juggling_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "A");
		pragma Assert (Swap_Count = 0);
	end;
	-- rotate 2
	declare
		Source : constant String := "BA";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		SA_Reversing.Reverse_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "AB");
		pragma Assert (Swap_Count = 1);
		Setup (Source);
		SA_Reversing.Juggling_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "AB");
		pragma Assert (Swap_Count = 1);
	end;
	-- rotate 3-1
	declare
		Source : constant String := "CAB";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		SA_Reversing.Reverse_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABC");
		pragma Debug (Report ("R3-1 rev"));
		Setup (Source);
		SA_Reversing.Juggling_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABC");
		pragma Debug (Report ("R3-1 jug"));
	end;
	-- rotate 3-2
	declare
		Source : constant String := "BCA";
		Middle : constant Integer := 2;
	begin
		Setup (Source);
		SA_Reversing.Reverse_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABC");
		pragma Debug (Report ("R3-2 rev"));
		Setup (Source);
		SA_Reversing.Juggling_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABC");
		pragma Debug (Report ("R3-2 jug"));
	end;
	-- rotate 4-1
	declare
		Source : constant String := "DABC";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		SA_Reversing.Reverse_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-1 rev"));
		Setup (Source);
		SA_Reversing.Juggling_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-1 jug"));
	end;
	-- rotate 4-2
	declare
		Source : constant String := "CDAB";
		Middle : constant Integer := 2;
	begin
		Setup (Source);
		SA_Reversing.Reverse_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-2 rev"));
		Setup (Source);
		SA_Reversing.Juggling_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-2 jug"));
	end;
	-- rotate 4-3
	declare
		Source : constant String := "BCDA";
		Middle : constant Integer := 3;
	begin
		Setup (Source);
		SA_Reversing.Reverse_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-3 rev"));
		Setup (Source);
		SA_Reversing.Juggling_Rotate_Elements (Data, Before => Middle + 1);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-3 jug"));
	end;
	-- sort a sorted data
	declare
		Source : constant String := "ABBCDDEFFG";
	begin
		Setup (Source);
		SA_Sorting.Insertion_Sort (Data);
		pragma Assert (Data.all = "ABBCDDEFFG");
		pragma Assert (Swap_Count = 0);
		pragma Debug (Report ("S1 ins"));
		Setup (Source);
		SA_Sorting.Merge_Sort (Data);
		pragma Assert (Data.all = "ABBCDDEFFG");
		pragma Assert (Swap_Count = 0);
		pragma Debug (Report ("S1 ipm"));
	end;
	-- sort a random data
	declare
		Source : constant String := "DBFGHIECJA";
	begin
		Setup (Source);
		SA_Sorting.Insertion_Sort (Data);
		pragma Assert (Data.all = "ABCDEFGHIJ");
		pragma Debug (Report ("S2 ins"));
		Setup (Source);
		SA_Sorting.Merge_Sort (Data);
		pragma Assert (Data.all = "ABCDEFGHIJ");
		pragma Debug (Report ("S2 ipm"));
	end;
	-- sort a random long data
	declare
		Source : constant String := "LOOOOOOOOOONGDATA";
	begin
		Setup (Source);
		SA_Sorting.Insertion_Sort (Data);
		pragma Assert (Data.all = "AADGLNOOOOOOOOOOT");
		pragma Debug (Report ("S3 ins"));
		Setup (Source);
		SA_Sorting.Merge_Sort (Data);
		pragma Assert (Data.all = "AADGLNOOOOOOOOOOT");
		pragma Debug (Report ("S3 ipm"));
	end;
	-- sort a keyboard
	declare
		Source : constant String := "ASDFGHJKL";
	begin
		Setup (Source);
		SA_Sorting.Insertion_Sort (Data);
		pragma Assert (Data.all = "ADFGHJKLS");
		pragma Debug (Report ("S4 ins"));
		Setup (Source);
		SA_Sorting.Merge_Sort (Data);
		pragma Assert (Data.all = "ADFGHJKLS");
		pragma Debug (Report ("S4 ipm"));
	end;
	Free (Data);
	pragma Debug (Ada.Debug.Put ("OK"));
end cntnr_array_sorting;
