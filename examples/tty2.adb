with Ada.Command_Line;
with Ada.Text_IO.Terminal;
procedure tty2 is
	use Ada.Text_IO;
	use Ada.Text_IO.Terminal;
	package Count_IO is new Integer_IO (Count);
	use Count_IO;
	Try_Resize, Try_Move, Try_Col, Try_Save : Boolean := False;
	State : Terminal.Output_State;
begin
	Count_IO.Default_Width := 0;
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		declare
			Arg : constant String := Ada.Command_Line.Argument (I);
		begin
			if Arg = "--resize" then
				Try_Resize := True;
			elsif Arg = "--move" then
				Try_Move := True;
			elsif Arg = "--col" then
				Try_Col := True;
			elsif Arg = "--save" then
				Try_Save := True;
			else
				Put (Standard_Error, "unknown option: "); Put (Standard_Error, Arg); New_Line (Standard_Error);
			end if;
		end;
	end loop;
	if Try_Save then
		Save_State (Standard_Output.all, State);
	end if;
	Put ("isatty(stdin) = "); Put (Boolean'Image (Is_Terminal (Standard_Input.all))); New_Line;
	Put ("isatty(stdout) = "); Put (Boolean'Image (Is_Terminal (Standard_Output.all))); New_Line;
	Put ("isatty(stderr) = "); Put (Boolean'Image (Is_Terminal (Standard_Error.all))); New_Line;
	if Try_Resize then
		Set_Size (Standard_Output.all, 75, 25);
	end if;
	declare
		S : Size_Type := Size (Standard_Output.all);
	begin
		Put ("size = "); Put (S.Line_Length); Put ("x"); Put (S.Page_Length); New_Line;
	end;
	declare
		V : View_Type := View (Standard_Output.all);
	begin
		Put ("view = ("); Put (V.Left); Put (", "); Put (V.Top);
		Put (")-("); Put (V.Right); Put (", "); Put (V.Bottom); Put (")"); New_Line;
	end;
	if Try_Move then
		Set_Position (Standard_Output.all, 20, 10);
	end if;
	if Try_Col then
		Terminal.Set_Col (Standard_Output.all, 30);
	end if;
	declare
		P : Position_Type := Position (Standard_Output.all);
	begin
		Put ("position = ("); Put (P.Col); Put (", "); Put (P.Line); Put (")"); New_Line;
	end;
	if Try_Save then
		Reset_State (Standard_Output.all, State);
	end if;
end tty2;
