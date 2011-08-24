with Ada.Real_Time;
with Ada.Synchronous_Barriers;
with Ada.Synchronous_Task_Control;
with Ada.Synchronous_Task_Control.EDF;
with System.Tasking.Inside;
procedure tasking2 is
begin
	declare
		ev : Ada.Synchronous_Task_Control.Suspension_Object;
		State : Boolean;
	begin
		pragma Assert (not Ada.Synchronous_Task_Control.Current_State (ev));
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
		Count : constant := 3;
		Start : Ada.Synchronous_Task_Control.Suspension_Object;
		Barrier : Ada.Synchronous_Barriers.Synchronous_Barrier (Count);
		procedure Process (Param : System.Address) is
			N : aliased Integer;
			for N'Address use Param;
			Notified : Boolean;
		begin
			Ada.Synchronous_Task_Control.Suspend_Until_True (Start);
			Ada.Synchronous_Barriers.Wait_For_Release (Barrier, Notified);
			Ada.Debug.Put (Integer'Image (N) & " : " & Boolean'Image (Notified));
		end Process;
		Ns : aliased array (1 .. Count) of aliased Integer;
	begin
		for I in Ns'Range loop
			Ns (I) := I;
			declare
				Id : System.Tasking.Inside.Task_Id;
			begin
				System.Tasking.Inside.Create (Id, Ns (I)'Address, Process'Access);
				System.Tasking.Inside.Detach (Id);
			end;
		end loop;
		Ada.Synchronous_Task_Control.Set_True (Start);
		delay 0.1;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking2;
