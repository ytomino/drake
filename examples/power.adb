with Ada;
procedure power is
	function Test1 (X, Y, Z : Integer) return Boolean is
	begin
		return X ** Y = Z;
	end Test1;
	function Test2 (X, Y, Z : Integer) return Boolean is
		pragma Unsuppress (Overflow_Check);
	begin
		return X ** Y = Z;
	end Test2;
begin
	pragma Assert (Test1 (2, 3, 8));
	pragma Assert (Test1 (3, 3, 27));
	pragma Assert (Test1 (-3, 3, -27));
	pragma Assert (Test2 (2, 3, 8));
	pragma Assert (Test2 (3, 3, 27));
	pragma Assert (Test2 (-3, 3, -27));
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end power;
