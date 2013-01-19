with Ada;
with System.Address_Image;
with System.Native_Stack;
procedure stackoverflow is
	procedure Put_Stack_Range is
		Stack_Top, Stack_Bottom : System.Address;
	begin
		System.Native_Stack.Get (Top => Stack_Top, Bottom => Stack_Bottom);
		Ada.Debug.Put ("top = " & System.Address_Image (Stack_Top));
		Ada.Debug.Put ("here = " & System.Address_Image (Stack_Top'Address));
		Ada.Debug.Put ("bottom = " & System.Address_Image (Stack_Bottom));
	end Put_Stack_Range;
	Count : Natural;
	procedure Do_Overflow is
		Big_Local : String (1 .. 1024 * 1024);
	begin
		Ada.Debug.Put ("enter");
		Count := Count + 1;
		-- begin jamming optimization before recursive
		Big_Local (1 .. 3) := Integer'Image (-99);
		-- end jamming optimization before recursive
		Do_Overflow;
		-- begin jamming optimization after recursive
		if Integer'Value (Big_Local (1 .. 3)) /= -99 then
			raise Program_Error;
		end if;
		-- end jamming optimization after recursive
		Ada.Debug.Put ("leave");
	end Do_Overflow;
	Count_1, Count_2 : Natural;
begin
	Put_Stack_Range;
	Ada.Debug.Put ("**** try 1 ****");
	Try_1 : begin
		Count := 0;
		Do_Overflow;
		raise Program_Error;
	exception
		when Storage_Error =>
			Ada.Debug.Put ("Storage_Error has raised on try 1");
			Count_1 := Count;
	end Try_1;
	Ada.Debug.Put ("**** try 2 ****");
	Try_2 : begin
		Count := 0;
		Do_Overflow;
		raise Program_Error;
	exception
		when Storage_Error =>
			Ada.Debug.Put ("Storage_Error has raised on try 2");
			Count_2 := Count;
	end Try_2;
	pragma Assert (Count_1 = Count_2);
	Ada.Debug.Put ("**** in task ****");
	Try_Task : declare
		task T;
		task body T is
		begin
			Put_Stack_Range;
			Ada.Debug.Put ("here is in task");
			Do_Overflow;
		exception
			when Storage_Error =>
				Ada.Debug.Put ("Storage_Error has raised in task");
		end T;
	begin
		null;
	end Try_Task;
	pragma Debug (Ada.Debug.Put ("OK"));
end stackoverflow;
