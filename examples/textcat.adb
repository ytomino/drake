with Ada.Text_IO; use Ada.Text_IO;
procedure textcat is
	Input : constant not null File_Access := Standard_Input;
	Output : constant not null File_Access := Standard_Output;
begin
	while not End_Of_File (Input.all) loop
		declare
			Item : Character;
			End_Of_Line : Boolean;
		begin
			Look_Ahead (Input.all, Item, End_Of_Line);
			if End_Of_Line then
				if End_Of_Page (Input.all) then
					New_Page (Output.all);
				else
					New_Line (Output.all);
				end if;
			else
				Put (Output.all, Item);
			end if;
			Skip_Ahead (Input.all);
		end;
	end loop;
end textcat;
