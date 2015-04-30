pragma License (Unrestricted);
--  extended unit
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO.Standard_Files;
private with C.sys.types;
package Ada.Processes is
   --  This package provides the way to execute new child process.

   type Process is limited private;

   procedure Create (
      Child : in out Process;
      Command_Line : String;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standard_Files.Standard_Input.all;
      Output : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standard_Files.Standard_Output.all;
      Error : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standard_Files.Standard_Error.all);
   function Create (
      Command_Line : String;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standard_Files.Standard_Input.all;
      Output : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standard_Files.Standard_Output.all;
      Error : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standard_Files.Standard_Error.all)
      return Process;

   procedure Wait (Child : Process; Status : out Command_Line.Exit_Status);
   procedure Wait (Child : Process);

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status);
   procedure Shell (
      Command_Line : String);

   procedure Append_Argument (
      Command_Line : in out String;
      Last : in out Natural;
      Argument : String);

   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   Device_Error : exception
      renames IO_Exceptions.Device_Error;

private

   type Process is new C.sys.types.pid_t;

end Ada.Processes;
