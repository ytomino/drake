with Ada.Unchecked_Conversion;
with Ada.Asynchronous_Task_Control;
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Task_Termination;
with System.Tasking.Inside;
procedure tasking3 is
	Count : constant := 3;
	function Cast is new Ada.Unchecked_Conversion (
		System.Tasking.Inside.Task_Id,
		Ada.Task_Identification.Task_Id);
	package Attr is new Ada.Task_Attributes (Integer, 0);
	procedure Process (Param : System.Address) is
	begin
		delay 0.1;
		Ada.Debug.Put (
			Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) &
			Integer'Image (Attr.Value));
	end Process;
	Ts : array (1 .. Count) of System.Tasking.Inside.Task_Id;
begin
	for I in Ts'Range loop
		System.Tasking.Inside.Create (Ts (I), System.Null_Address, Process'Access);
		Attr.Set_Value (I, T => Cast (Ts (I)));
	end loop;
	for I in Ts'Range loop
		System.Tasking.Inside.Wait (Ts (I));
	end loop;
	Ada.Debug.Put ("ok");
end tasking3;
