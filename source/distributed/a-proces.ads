pragma License (Unrestricted);
--  extended unit
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO.Standard_Files;
private with System.Native_Processes;
package Ada.Processes is
   --  This package provides the way to execute new child process.

   procedure Append_Argument (
      Command_Line : in out String;
      Last : in out Natural;
      Argument : String);
   pragma Inline (Append_Argument); -- renamed

   type Process is limited private;

--  subtype Open_Process is Process
--    with
--       Dynamic_Predicate => Is_Open (Open_Process),
--       Predicate_Failure => raise Status_Error;

   --  Child process management

   function Is_Open (Child : Process) return Boolean;
   pragma Inline (Is_Open); -- renamed

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

   procedure Wait (
      Child : in out Process; -- Open_Process
      Status : out Command_Line.Exit_Status);
   procedure Wait (
      Child : in out Process); -- Open_Process
   pragma Inline (Wait);

   procedure Wait_Immediate (
      Child : in out Process; -- Open_Process
      Terminated : out Boolean;
      Status : out Command_Line.Exit_Status);
   procedure Wait_Immediate (
      Child : in out Process; -- Open_Process
      Terminated : out Boolean);
   pragma Inline (Wait_Immediate);

   procedure Abort_Process (Child : in out Process); -- Open_Process
   procedure Forced_Abort_Process (Child : in out Process); -- Open_Process

   pragma Inline (Abort_Process);
   pragma Inline (Forced_Abort_Process);

   --  Pass a command to the shell

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status);
   procedure Shell (
      Command_Line : String);
   pragma Inline (Shell); -- renamed, or for shorthand

   --  Exceptions

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   Device_Error : exception
      renames IO_Exceptions.Device_Error;

private

   procedure Append_Argument (
      Command_Line : in out String;
      Last : in out Natural;
      Argument : String)
      renames System.Native_Processes.Append_Argument;

   type Process is new System.Native_Processes.Process;

   function Is_Open (Child : Process) return Boolean
      renames Do_Is_Open; -- inherited

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status)
      renames System.Native_Processes.Shell;

end Ada.Processes;
