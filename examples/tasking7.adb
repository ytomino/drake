with Ada.Calendar;
with Ada.Synchronous_Task_Control;
with System.Native_Tasks.Yield;
with System.Tasks;
procedure tasking7 is
	use type Ada.Calendar.Time;
begin
	Ada.Debug.Put ("**** break Suspend_Until_True ****");
	declare
		ev : Ada.Synchronous_Task_Control.Suspension_Object;
		task T1;
		task body T1 is
		begin
			-- force switching to environment task
			for I in 1 .. 500 loop
				System.Native_Tasks.Yield;
			end loop;
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
			-- force switching to environment task
			for I in 1 .. 500 loop
				System.Native_Tasks.Yield;
			end loop;
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
	Ada.Debug.Put ("**** break a task having a child task ****");
	declare
		task T3;
		task body T3 is
			task T3_Child is
			end T3_Child;
			task body T3_Child is
			begin
				Ada.Debug.Put ("before wait in nested task");
				delay 999.9;
				Ada.Debug.Put ("after wait in nested task");
				raise Program_Error; -- it does not come here
			exception
				when Standard'Abort_Signal =>
					System.Tasks.When_Abort_Signal;
					Ada.Debug.Put ("aborted in nested task");
					raise;
			end T3_Child;
		begin
			Ada.Debug.Put ("before wait in task");
			delay 999.9;
			Ada.Debug.Put ("after wait in task");
			raise Program_Error; -- it does not come here
		exception
			when Standard'Abort_Signal =>
				System.Tasks.When_Abort_Signal;
				Ada.Debug.Put ("aborted in task");
				raise;
		end T3;
	begin
		Ada.Debug.Put ("before abort");
		abort T3;
		Ada.Debug.Put ("after abort");
	end;
	Ada.Debug.Put ("**** break a task waiting an other tasks ****");
	declare
		task T4;
		task body T4 is
		begin
			declare
				task T4_Child is
				end T4_Child;
				task body T4_Child is
				begin
					Ada.Debug.Put ("before wait in nested task");
					delay 999.9;
					Ada.Debug.Put ("after wait in nested task");
					raise Program_Error; -- it does not come here
				exception
					when Standard'Abort_Signal =>
						System.Tasks.When_Abort_Signal;
						Ada.Debug.Put ("aborted in nested task");
						raise;
				end T4_Child;
			begin
				Ada.Debug.Put ("before wait in task");
			end; -- wait T4_Child in Leave_Master called from here
			Ada.Debug.Put ("after wait in task");
			delay 0.0; -- abort checking
			raise Program_Error; -- it does not come here
		exception
			when Standard'Abort_Signal =>
				System.Tasks.When_Abort_Signal;
				Ada.Debug.Put ("aborted in task");
				raise;
		end T4;
	begin
		delay 0.1; -- wait until T4 has waited T4_Child
		Ada.Debug.Put ("before abort");
		abort T4;
		Ada.Debug.Put ("after abort");
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking7;
