with Ada.Enumeration;
procedure enum is
	package CA is new Ada.Enumeration.Arithmetic (Character, Integer);
	use CA;
begin
	pragma Assert ('A' + 1 = 'B');
	pragma Assert ('B' - 1 = 'A');
	pragma Assert ('a' - 'A' = 32);
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end enum;
