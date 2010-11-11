with Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Bounded;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Bounded;
with Ada.Strings.Wide_Wide_Unbounded;
procedure str is
	X : String := "11";
begin
	pragma Assert (Ada.Strings.Fixed.Index ("abc", 'b', 1) = 2);
	pragma Assert (Ada.Strings.Fixed.Index ("abc", 'd', 1) = 0);
	pragma Assert (Ada.Strings.Fixed.Index ("abcabc", 'b', 3) = 5);
	pragma Assert (Ada.Strings.Fixed.Index ("aaabbb", "bbb", 1) = 4);
	pragma Assert (Ada.Strings.Fixed.Index ("aaabbb", "aaa", 6, Ada.Strings.Backward) = 1);
	pragma Assert (X > "0");
	pragma Assert (X < "2");
	pragma Assert (X > "10");
	pragma Assert (X < "12");
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end str;
