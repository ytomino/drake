with Ada.Real_Time;
with Ada.Synchronous_Barriers;
with Ada.Synchronous_Task_Control;
with Ada.Synchronous_Task_Control.EDF;
with System.Storage_Elements;
with System.Tasks;
procedure tasking2 is
begin
	declare
		ev : Ada.Synchronous_Task_Control.Suspension_Object;
		State : Boolean;
	begin
--		pragma Assert (not Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.Set_True (ev);
		pragma Assert (Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.Suspend_Until_True (ev);
		pragma Assert (Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			ev,
			Ada.Real_Time.To_Time_Span (1.0),
			State);
		pragma Assert (State);
		pragma Assert (Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.Set_False (ev);
		pragma Assert (not Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			ev,
			Ada.Real_Time.To_Time_Span (1.0),
			State); -- it may be timeout
		pragma Assert (not State);
		pragma Assert (not Ada.Synchronous_Task_Control.Current_State (ev));
	end;
	declare
		Try_Count : constant := 2;
		Task_Count : constant := 3;
		Start : Ada.Synchronous_Task_Control.Suspension_Object;
		Barrier : Ada.Synchronous_Barriers.Synchronous_Barrier (Task_Count);
		protected Notified_Count is
			pragma Lock_Free;
			procedure Increment;
			function Get return Integer;
		private
			Value : Integer := 0;
		end Notified_Count;
		protected body Notified_Count is
			procedure Increment is
			begin
				Value := Value + 1;
			end Increment;
			function Get return Integer is
			begin
				return Value;
			end Get;
		end Notified_Count;
		procedure Process (Param : System.Address) is
			N : constant System.Storage_Elements.Integer_Address :=
				System.Storage_Elements.To_Integer (Param);
			Notified : Boolean;
		begin
			Ada.Synchronous_Task_Control.Suspend_Until_True (Start, Multi => True);
			Ada.Synchronous_Barriers.Wait_For_Release (Barrier, Notified);
			Ada.Debug.Put (
				System.Storage_Elements.Integer_Address'Image (N)
				& " : "
				& Boolean'Image (Notified));
			if Notified then
			   Notified_Count.Increment;
			end if;
		end Process;
	begin
		for Trying in 1 .. Try_Count loop
			Ada.Synchronous_Task_Control.Set_False (Start);
			for I in 1 .. Task_Count loop
				declare
					N : constant System.Storage_Elements.Integer_Address :=
						System.Storage_Elements.Integer_Address (Trying * 10 + I);
					Id : System.Tasks.Task_Id;
				begin
					System.Tasks.Create (
						Id,
						System.Storage_Elements.To_Address (N),
						Process'Access);
					System.Tasks.Detach (Id);
				end;
			end loop;
			Ada.Synchronous_Task_Control.Set_True (Start);
		end loop;
		delay 0.95; -- note, currently, Abort_Checking_Span = 1.0
		pragma Assert (Notified_Count.Get = Try_Count);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking2;
