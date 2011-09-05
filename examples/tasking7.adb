with Ada.Calendar;
with Ada.Synchronous_Task_Control;
procedure tasking7 is
	use type Ada.Calendar.Time;
begin
	Ada.Debug.Put ("**** break Suspend_Until_True ****");
	declare
		ev : Ada.Synchronous_Task_Control.Suspension_Object;
		task T1;
		task body T1 is
		begin
			Ada.Debug.Put ("before wait in task");
			Ada.Synchronous_Task_Control.Suspend_Until_True (ev);
			Ada.Debug.Put ("after wait in task");
			raise Program_Error; -- it does not come here
		end T1;
	begin
		Ada.Debug.Put ("before abort");
		abort T1;
		Ada.Debug.Put ("after abort");
	end;
	Ada.Debug.Put ("**** break delay ****");
	declare
		task T2;
		task body T2 is
		begin
			Ada.Debug.Put ("before wait in task");
			delay until Ada.Calendar.Clock + 999.9;
			Ada.Debug.Put ("after wait in task");
			raise Program_Error; -- it does not come here
		end T2;
	begin
		Ada.Debug.Put ("before abort");
		abort T2;
		Ada.Debug.Put ("after abort");
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking7;
