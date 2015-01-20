with Ada.Task_Identification;
procedure task_caller is
	use type Ada.Task_Identification.Task_Id;
	-- spec of callee
	task Callee is
		entry Level_1;
		entry Level_2;
		entry Level_3;
	end Callee;
	-- spec of callers
	task Caller_1 is
		entry Finish;
	end Caller_1;
	task Caller_2 is
		entry Finish;
	end Caller_2;
	-- body of callee
	task body Callee is
	begin
		accept Level_1 do
			pragma Assert (Level_1'Caller = Caller_1'Identity);
			accept Level_2 do
				pragma Assert (Level_1'Caller = Caller_1'Identity);
				pragma Assert (Level_2'Caller = Caller_2'Identity);
				accept Level_3 do
					pragma Assert (Level_1'Caller = Caller_1'Identity);
					pragma Assert (Level_2'Caller = Caller_2'Identity);
					pragma Assert (Level_3'Caller = Ada.Task_Identification.Environment_Task);
					null;
				end Level_3;
			end Level_2;
			Caller_2.Finish;
		end Level_1;
		Caller_1.Finish;
	end Callee;
	-- bodies of callers
	task body Caller_1 is
	begin
		Callee.Level_1;
		accept Finish do
			pragma Assert (Finish'Caller = Callee'Identity);
			null;
		end Finish;
	end Caller_1;
	task body Caller_2 is
	begin
		Callee.Level_2;
		accept Finish do
			pragma Assert (Finish'Caller = Callee'Identity);
			null;
		end Finish;
	end Caller_2;
begin
	Callee.Level_3;
	pragma Debug (Ada.Debug.Put ("OK"));
end task_caller;
