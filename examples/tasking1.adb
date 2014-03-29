with Ada;
with System.Tasks; -- primitives
procedure tasking1 is
	procedure Process (Param : System.Address) is
		N : aliased Integer;
		for N'Address use Param;
	begin
		delay 0.1;
		Ada.Debug.Put (Integer'Image (N));
	end Process;
	N1 : aliased constant Integer := 1;
	N2 : aliased constant Integer := 2;
	Id1 : System.Tasks.Task_Id;
	Id2 : System.Tasks.Task_Id;
	Aborted : Boolean;
begin
	System.Tasks.Create (Id1, N1'Address, Process'Access);
	System.Tasks.Create (Id2, N2'Address, Process'Access);
	System.Tasks.Wait (Id1, Aborted => Aborted);
	System.Tasks.Wait (Id2, Aborted => Aborted);
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking1;
