-- Demonstration for the behaviors of unhandled exceptions.
with Ada.Exceptions;
with Ada.Interrupts.Names;
with Ada.Task_Identification;
with Ada.Task_Termination;
procedure exception_unhandled is
begin
	-- terminate a task by unhandled exception
	declare
		task T;
		task body T is
		begin
			Ada.Debug.Put ("in task T");
			raise Program_Error; -- will be reported by default
		end T;
	begin
		null;
	end;
	declare -- it can replace the report by a termination handler
		Handled : Boolean := False;
		protected Handlers is
			procedure Handle_T2 (
				Cause : Ada.Task_Termination.Cause_Of_Termination;
				T : Ada.Task_Identification.Task_Id;
				X : Ada.Exceptions.Exception_Occurrence);
		end Handlers;
		protected body Handlers is
			procedure Handle_T2 (
				Cause : Ada.Task_Termination.Cause_Of_Termination;
				T : Ada.Task_Identification.Task_Id;
				X : Ada.Exceptions.Exception_Occurrence) is
			begin
				case Cause is
					when Ada.Task_Termination.Normal
						| Ada.Task_Termination.Abnormal =>
						null;
					when Ada.Task_Termination.Unhandled_Exception =>
						Ada.Debug.Put (Ada.Exceptions.Exception_Name (X));
						Handled := True;
				end case;
			end Handle_T2;
		end Handlers;
	begin
		declare
			task T2;
			task body T2 is
			begin
				Ada.Debug.Put ("in task T2");
				Ada.Task_Termination.Set_Specific_Handler (
					T2'Identity,
					Handlers.Handle_T2'Unrestricted_Access);
				raise Program_Error; -- will be handled by Handlers.Handle_T2
			end T2;
		begin
			null;
		end;
		pragma Assert (Handled);
	end;
	-- terminate a interrupt handler by unhandled exception
	declare
		protected Handlers is
			procedure Handle_SIGINT;
		end Handlers;
		protected body Handlers is
			procedure Handle_SIGINT is
			begin
				Ada.Debug.Put ("in interrupt SIGINT");
				raise Program_Error;
			end Handle_SIGINT;
		end Handlers;
	begin
		Ada.Interrupts.Unchecked_Attach_Handler (Handlers.Handle_SIGINT'Unrestricted_Access, Ada.Interrupts.Names.SIGINT);
		Ada.Interrupts.Raise_Interrupt (Ada.Interrupts.Names.SIGINT);
	end;
	Ada.Debug.Put ("in environment task");
	raise Program_Error;
end exception_unhandled;
