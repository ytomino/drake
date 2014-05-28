with Ada.Interrupts.Names;
procedure exception_unhandled is
begin
	-- terminate a task by unhandled exception
	declare
		task T;
		task body T is
		begin
			Ada.Debug.Put ("in task T");
			raise Program_Error;
		end T;
	begin
		null;
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
