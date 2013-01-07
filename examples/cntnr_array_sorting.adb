with Ada.Containers.Inside.Array_Sorting;
with Ada.Unchecked_Deallocation;
with System;
procedure cntnr_array_sorting is
	package AS renames Ada.Containers.Inside.Array_Sorting;
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
	function LT (Left, Right : Integer; Params : System.Address) return Boolean is
		pragma Debug (Increment (Comp_Count));
	begin
		return Data (Left) < Data (Right);
	end LT;
	procedure Swap (Left, Right : Integer; Params : System.Address) is
		pragma Debug (Increment (Swap_Count));
		Temp : Character := Data (Left);
	begin
		Data (Left) := Data (Right);
		Data (Right) := Temp;
	end Swap;
begin
	-- Is_Sorted
	begin
		Setup ("");
		pragma Assert (AS.Is_Sorted (Data'First, Data'Last, System.Null_Address, LT'Access));
		Setup ("ABCDEF");
		pragma Assert (AS.Is_Sorted (Data'First, Data'Last, System.Null_Address, LT'Access));
		Setup ("ABCCDD");
		pragma Assert (AS.Is_Sorted (Data'First, Data'Last, System.Null_Address, LT'Access));
		Setup ("ABCBA");
		pragma Assert (not AS.Is_Sorted (Data'First, Data'Last, System.Null_Address, LT'Access));
	end;
	-- In_Place_Reverse
	begin
		Setup ("");
		AS.In_Place_Reverse (Data'First, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "");
		pragma Assert (Swap_Count = 0);
		Setup ("A");
		AS.In_Place_Reverse (Data'First, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "A");
		pragma Assert (Swap_Count = 0);
		Setup ("BA");
		AS.In_Place_Reverse (Data'First, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "AB");
		pragma Assert (Swap_Count = 1);
		Setup ("CBA");
		AS.In_Place_Reverse (Data'First, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABC");
		pragma Assert (Swap_Count = 1);
		Setup ("DCBA");
		AS.In_Place_Reverse (Data'First, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABCD");
		pragma Assert (Swap_Count = 2);
	end;
	-- rotate an empty data
	declare
		Source : constant String := "";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		AS.Reverse_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "");
		pragma Assert (Swap_Count = 0);
		Setup (Source);
		AS.Juggling_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "");
		pragma Assert (Swap_Count = 0);
	end;
	-- rotate 1
	declare
		Source : constant String := "A";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		AS.Reverse_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "A");
		pragma Assert (Swap_Count = 0);
		Setup (Source);
		AS.Juggling_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "A");
		pragma Assert (Swap_Count = 0);
	end;
	-- rotate 2
	declare
		Source : constant String := "BA";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		AS.Reverse_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "AB");
		pragma Assert (Swap_Count = 1);
		Setup (Source);
		AS.Juggling_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "AB");
		pragma Assert (Swap_Count = 1);
	end;
	-- rotate 3-1
	declare
		Source : constant String := "CAB";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		AS.Reverse_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABC");
		pragma Debug (Report ("R3-1 rev"));
		Setup (Source);
		AS.Juggling_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABC");
		pragma Debug (Report ("R3-1 jug"));
	end;
	-- rotate 3-2
	declare
		Source : constant String := "BCA";
		Middle : constant Integer := 2;
	begin
		Setup (Source);
		AS.Reverse_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABC");
		pragma Debug (Report ("R3-2 rev"));
		Setup (Source);
		AS.Juggling_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABC");
		pragma Debug (Report ("R3-2 jug"));
	end;
	-- rotate 4-1
	declare
		Source : constant String := "DABC";
		Middle : constant Integer := 1;
	begin
		Setup (Source);
		AS.Reverse_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-1 rev"));
		Setup (Source);
		AS.Juggling_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-1 jug"));
	end;
	-- rotate 4-2
	declare
		Source : constant String := "CDAB";
		Middle : constant Integer := 2;
	begin
		Setup (Source);
		AS.Reverse_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-2 rev"));
		Setup (Source);
		AS.Juggling_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-2 jug"));
	end;
	-- rotate 4-3
	declare
		Source : constant String := "BCDA";
		Middle : constant Integer := 3;
	begin
		Setup (Source);
		AS.Reverse_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-3 rev"));
		Setup (Source);
		AS.Juggling_Rotate (Data'First, Middle, Data'Last, System.Null_Address, Swap'Access);
		pragma Assert (Data.all = "ABCD");
		pragma Debug (Report ("R4-3 jug"));
	end;
	-- sort a sorted data
	declare
		Source : constant String := "ABBCDDEFFG";
	begin
		Setup (Source);
		AS.Insertion_Sort (Data'First, Data'Last, System.Null_Address, LT'Access, Swap'Access);
		pragma Assert (Data.all = "ABBCDDEFFG");
		pragma Assert (Swap_Count = 0);
		pragma Debug (Report ("S1 ins"));
		Setup (Source);
		AS.In_Place_Merge_Sort (Data'First, Data'Last, System.Null_Address, LT'Access, Swap'Access);
		pragma Assert (Data.all = "ABBCDDEFFG");
		pragma Assert (Swap_Count = 0);
		pragma Debug (Report ("S1 ipm"));
	end;
	-- sort a random data
	declare
		Source : constant String := "DBFGHIECJA";
	begin
		Setup (Source);
		AS.Insertion_Sort (Data'First, Data'Last, System.Null_Address, LT'Access, Swap'Access);
		pragma Assert (Data.all = "ABCDEFGHIJ");
		pragma Debug (Report ("S2 ins"));
		Setup (Source);
		AS.In_Place_Merge_Sort (Data'First, Data'Last, System.Null_Address, LT'Access, Swap'Access);
		pragma Assert (Data.all = "ABCDEFGHIJ");
		pragma Debug (Report ("S2 ipm"));
	end;
	-- sort a random long data
	declare
		Source : constant String := "LOOOOOOOOOONGDATA";
	begin
		Setup (Source);
		AS.Insertion_Sort (Data'First, Data'Last, System.Null_Address, LT'Access, Swap'Access);
		pragma Assert (Data.all = "AADGLNOOOOOOOOOOT");
		pragma Debug (Report ("S3 ins"));
		Setup (Source);
		AS.In_Place_Merge_Sort (Data'First, Data'Last, System.Null_Address, LT'Access, Swap'Access);
		pragma Assert (Data.all = "AADGLNOOOOOOOOOOT");
		pragma Debug (Report ("S3 ipm"));
	end;
	-- sort a keyboard
	declare
		Source : constant String := "ASDFGHJKL";
	begin
		Setup (Source);
		AS.Insertion_Sort (Data'First, Data'Last, System.Null_Address, LT'Access, Swap'Access);
		pragma Assert (Data.all = "ADFGHJKLS");
		pragma Debug (Report ("S4 ins"));
		Setup (Source);
		AS.In_Place_Merge_Sort (Data'First, Data'Last, System.Null_Address, LT'Access, Swap'Access);
		pragma Assert (Data.all = "ADFGHJKLS");
		pragma Debug (Report ("S4 ipm"));
	end;
	Free (Data);
	pragma Debug (Ada.Debug.Put ("OK"));
end cntnr_array_sorting;
