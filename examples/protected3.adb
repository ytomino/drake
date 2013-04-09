with Ada.Task_Identification;
procedure protected3 is
	use type Ada.Task_Identification.Task_Id;
begin
	Ada.Debug.Put ("entries only");
	declare
		protected CS1 is
			entry Enter;
			entry Leave;
		private
			Locked : Boolean := False;
		end CS1;
		protected body CS1 is
			entry Enter when not Locked is
			begin
				Locked := True;
			end Enter;
			entry Leave when Locked is
			begin
				Locked := False;
			end Leave;
		end CS1;
	begin
		CS1.Enter;
		CS1.Leave;
	end;
	Ada.Debug.Put ("entries and subprograms");
	declare
		protected CS2 is
			entry Enter (Current_Task : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Current_Task);
			procedure Leave;
			function Locked return Boolean;
		private
			entry Wait (Current_Task : Ada.Task_Identification.Task_Id);
			Lock_Count : Natural := 0;
			Owner : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Null_Task_Id;
		end CS2;
		protected body CS2 is
			entry Enter (Current_Task : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Current_Task) when True is
			begin
				if Owner = Current_Task then
					Lock_Count := Lock_Count + 1;
				else
					requeue Wait;
				end if;
			end Enter;
			procedure Leave is
			begin
				Lock_Count := Lock_Count - 1;
				if Lock_Count = 0 then
					Owner := Ada.Task_Identification.Null_Task_Id;
				end if;
			end Leave;
			function Locked return Boolean is
			begin
				return Lock_Count > 0;
			end Locked;
			entry Wait (Current_Task : Ada.Task_Identification.Task_Id) when Lock_Count = 0 is
			begin
				Lock_Count := 1;
				Owner := Current_Task;
			end Wait;
		end CS2;
	begin
		CS2.Enter;
		pragma Assert (CS2.Locked);
		CS2.Leave;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end protected3;
