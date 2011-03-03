pragma License (Unrestricted);
--  extended package
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO.Standards;
private with C.sys.types;
package Ada.Processes is

   type Process is limited private;

   procedure Create (
      Child : in out Process;
      Command_Line : String;
      Directory : String := "";
      Input : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Input.all;
      Output : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Output.all;
      Error : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Error.all);
   function Create (
      Command_Line : String;
      Directory : String := "";
      Input : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Input.all;
      Output : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Output.all;
      Error : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Error.all)
      return Process;

   procedure Wait (Child : Process; Status : out Command_Line.Exit_Status);
   procedure Wait (Child : Process);

   procedure Shell (Command : String; Status : out Command_Line.Exit_Status);
   procedure Shell (Command : String);

   Name_Error : exception renames IO_Exceptions.Name_Error;
   Use_Error : exception renames IO_Exceptions.Use_Error;

private

   type Process is new C.sys.types.pid_t;

end Ada.Processes;
