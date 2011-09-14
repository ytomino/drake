with Ada.Containers.Generic_Arrays;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
procedure cntnr_Array is
	type String_Access is access String;
	procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
	package Arrays is new Ada.Containers.Generic_Arrays (
		Positive, Character, String, String_Access);
	function "&" (Left : String_Access; Right : Character)
		return Arrays.New_Array
		renames Arrays."&";
	procedure Test_01 is
		package Sorting is new Arrays.Generic_Sorting;
		Data : String_Access := new String'("asdfghjkl");
	begin
		Sorting.Sort (Data);
		--Ada.Text_IO.Put_Line(Data.all);
		pragma Assert (Sorting.Is_Sorted (Data));
		Free (Data);
	end Test_01;
	pragma Debug (Test_01);
	procedure Test_02 is
		use type Ada.Containers.Count_Type;
		X : String_Access := new String'("ABC");
		Y : String_Access;
	begin
		pragma Assert (Arrays.Length (X) = 3);
		Arrays.Assign (Y, X & 'D');
		pragma Assert (Arrays.Length (Y) = 4);
		pragma Assert (Y.all = "ABCD");
		Free (X);
		Free (Y);
	end Test_02;
	pragma Debug (Test_02);
	procedure Test_03 is
		use type Ada.Containers.Count_Type;
		X : aliased String_Access := new String'("ABCD");
	begin
		Arrays.Delete (X, 2, 2);
		pragma Assert (X.all = "AD");
		Arrays.Insert (X, 2, 'Z');
		pragma Assert (X.all = "AZD");
		Arrays.Append (X, 'a');
		pragma Assert (X.all = "AZDa");
		Arrays.Prepend (X, 'p');
		pragma Assert (X.all = "pAZDa");
		Arrays.Delete_First (X);
		Arrays.Delete_Last (X);
		pragma Assert (X.all = "AZD");
		Free (X);
	end Test_03;
	pragma Debug (Test_03);
	pragma Debug (Ada.Debug.Put ("OK"));
begin
	null;
end cntnr_Array;
