with Ada;
procedure protected_lf is
	protected prot is
		pragma Lock_Free;
		procedure w (arg : Integer);
		function r return Integer;
	private
		value : Integer := 0;
	end prot;
	protected body prot is
		procedure w (arg: Integer) is
		begin
			value := arg;
		end w;
		function r return Integer is
		begin
			return value;
		end r;
	end prot;
	V : Integer;
begin
	prot.w (100);
	V := prot.r;
	pragma Assert (V = 100);
	pragma Debug (Ada.Debug.Put ("OK"));
end protected_lf;
