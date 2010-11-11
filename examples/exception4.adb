procedure exception4 is
begin
	raise Program_Error;
exception
	when Program_Error =>
		begin
			raise Program_Error;
		exception
			when Program_Error =>
				null;
		end;
end exception4;
