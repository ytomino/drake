with Ada;
with System.Tasking.Inside; -- primitives
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
	Id1 : System.Tasking.Inside.Task_Id;
	Id2 : System.Tasking.Inside.Task_Id;
begin
	System.Tasking.Inside.Create (Id1, N1'Address, Process'Access);
	System.Tasking.Inside.Create (Id2, N2'Address, Process'Access);
	System.Tasking.Inside.Wait (Id1);
	System.Tasking.Inside.Wait (Id2);
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking1;
