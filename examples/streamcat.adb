with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams.Stream_IO.Standard_Files; use Ada.Streams.Stream_IO.Standard_Files;
procedure streamcat is
	E : Stream_Element_Array (1 .. 1);
	Last : Stream_Element_Offset;
begin
	while not End_Of_File (Standard_Input.all) loop
		Read (Standard_Input.all, E, Last);
		Write (Standard_Output.all, E);
	end loop;
end streamcat;
