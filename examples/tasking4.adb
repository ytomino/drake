with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with System.Tasking.Tasks;
procedure tasking4 is
	package Lists is new Ada.Containers.Doubly_Linked_Lists (Character);
	procedure Display (C : Lists.List) is
		type SA is access String;
		for SA'Storage_Size use 50; -- test System.Pool_Size
		procedure Free is new Ada.Unchecked_Deallocation (String, SA);
		Pic : SA := new String (1 .. C.Length);
		Index : Positive := Pic'First;
		Local_Error : exception;
		Z : Lists.List := C; -- test controlled type in task
		I : Lists.Cursor := Z.First;
	begin
		while Lists.Has_Element (I) loop
			Pic (Index) := Lists.Element (I);
			Index := Index + 1;
			Lists.Next (I);
		end loop;
		begin
			Ada.Debug.Put (Pic.all);
			raise Local_Error; -- test exception in task
		exception
			when Local_Error => Free (Pic);
		end;
	end Display;
	X, Y : Lists.List;
	procedure Process (Param : System.Address) is
	begin
		for I in Character'('D') .. 'Z' loop
			Lists.Append (Y, I); -- break sharing
		end loop;
		Display (Y);
	end Process;
	T : System.Tasking.Tasks.Task_Id;
	Aborted : Boolean;
begin
	for I in Character'('A') .. 'C' loop
		Lists.Append (X, I);
	end loop;
	Y := X; -- sharing
	System.Tasking.Tasks.Create (T, System.Null_Address, Process'Access);
	for I in Character'('D') .. 'Z' loop
		Lists.Append (X, I); -- break sharing
	end loop;
	Display (X);
	System.Tasking.Tasks.Wait (T, Aborted => Aborted);
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking4;
