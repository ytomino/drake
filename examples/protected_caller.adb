with Ada.Task_Identification;
procedure protected_caller is
	use type Ada.Task_Identification.Task_Id;
	-- spec of callee
	protected Callee is
		entry Entry_1;
		entry Entry_2;
		entry Entry_3;
	end Callee;
	-- spec of callers
	task Caller_1 is
	end Caller_1;
	task Caller_2 is
	end Caller_2;
	-- body of callee
	protected body Callee is
		entry Entry_1 when True is
		begin
			pragma Assert (Entry_1'Caller = Caller_1'Identity);
			null;
		end Entry_1;
		entry Entry_2 when True is
		begin
			pragma Assert (Entry_2'Caller = Caller_2'Identity);
			null;
		end Entry_2;
		entry Entry_3 when True is
		begin
			pragma Assert (Entry_3'Caller = Ada.Task_Identification.Environment_Task);
			null;
		end Entry_3;
	end Callee;
	-- bodies of callers
	task body Caller_1 is
	begin
		Callee.Entry_1;
	end Caller_1;
	task body Caller_2 is
	begin
		Callee.Entry_2;
	end Caller_2;
begin
	Callee.Entry_3;
	pragma Debug (Ada.Debug.Put ("OK"));
end protected_caller;
