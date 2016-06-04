pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Naked_Stream_IO;
with C.sys.types;
package System.Native_Processes is
   use type C.sys.types.pid_t;

   procedure Append_Argument (
      Command_Line : in out String;
      Last : in out Natural;
      Argument : String);

   --  Child process type

   type Process is limited record
      Id : C.sys.types.pid_t := -1;
   end record;

   --  Child process management

   function Do_Is_Open (Child : Process) return Boolean;
   pragma Inline (Do_Is_Open);

   procedure Create (
      Child : in out Process;
      Command_Line : String;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Output : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Error : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type);

   procedure Do_Wait (
      Child : in out Process;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Do_Wait_Immediate (
      Child : in out Process;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Do_Abort_Process (Child : in out Process);
   procedure Do_Forced_Abort_Process (Child : in out Process);

   --  Pass a command to the shell

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status);

   --  Exceptions

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception
      renames Ada.IO_Exceptions.Device_Error;

end System.Native_Processes;
