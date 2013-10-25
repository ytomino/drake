with Ada.Directories.Temporary;
with Ada.Streams.Stream_IO;
procedure filelock is
	use Ada.Streams.Stream_IO;
	Name : String := Ada.Directories.Temporary.Create_Temporary_File;
begin
	-- raising
	declare
		Step : Integer := 0;
		File_1, File_2 : File_Type;
	begin
		Step := 1;
		Open (File_1, In_File, Name); -- read lock
		Step := 2;
		Open (File_2, In_File, Name, Form => "shared=write"); -- write lock
		Step := 3;
	exception
		when Tasking_Error =>
			if Step /= 2 then
				Ada.Debug.Put ("bad");
			end if;
	end;
	-- waiting
	declare
		Step : Integer := 0;
		pragma Atomic (Step);
		File_1 : File_Type;
		task Task_2 is
		end Task_2;
		task body Task_2 is
			File_2 : File_Type;
		begin
			delay 0.1;
			pragma Assert (Step = 2);
			Step := 3;
			Open (File_2, In_File, Name, Form => "shared=write,race=wait"); -- write lock
			pragma Assert (Step = 4);
			Step := 5;
		end Task_2;
	begin
		pragma Assert (Step = 0);
		Step := 1;
		Open (File_1, In_File, Name, "race=wait"); -- read lock
		pragma Assert (Step = 1);
		Step := 2;
		delay 0.2;
		pragma Assert (Step = 3);
		Step := 4;
		Close (File_1);
	end;
	Ada.Directories.Delete_File (Name);
	Ada.Debug.Put ("OK");
end filelock;
