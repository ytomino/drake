with Ada.Direct_IO;
with Ada.Sequential_IO;
with Ada.Storage_IO;
procedure typedio is
	package Character_IO is new Ada.Direct_IO (Character);
	package Duration_IO is new Ada.Sequential_IO (Duration);
	package Integer_IO is new Ada.Storage_IO (Integer);
begin
	Test_Direct_IO : declare
		use Character_IO;
		File : File_Type;
		Temp_Name : access String;
		Buf : Character;
	begin
		Create (File, Inout_File);
		Temp_Name := new String'(Name (File)); -- name of temporary file
		pragma Debug (Ada.Debug.Put (Temp_Name.all));
		pragma Assert (Index (File) = 1);
		pragma Assert (Size (File) = 0);
		Write (File, '1');
		Write (File, '2');
		Write (File, '3');
		pragma Assert (Index (File) = 4);
		Set_Index (File, 1);
		pragma Assert (Index (File) = 1);
		pragma Assert (Size (File) = 3);
		Read (File, Buf);
		pragma Assert (Buf = '1');
		Write (File, 'B');
		Read (File, Buf);
		pragma Assert (Buf = '3');
		Set_Index (File, 1);
		Read (File, Buf);
		pragma Assert (Buf = '1');
		Read (File, Buf);
		pragma Assert (Buf = 'B');
		Read (File, Buf);
		pragma Assert (Buf = '3');
		pragma Assert (Index (File) = 4);
		pragma Assert (Size (File) = 3);
		pragma Assert (End_Of_File (File));
		Close (File);
		begin
			Open (File, In_File, Temp_Name.all);
			pragma Debug (Ada.Debug.Put ("the temporary file is not deleted!"));
			raise Program_Error;
		exception
			when Name_Error => null;
		end;
	end Test_Direct_IO;
	pragma Debug (Ada.Debug.Put ("OK"));
end typedio;
