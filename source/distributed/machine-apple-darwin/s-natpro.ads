pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Naked_Stream_IO;
with C.sys.types;
package System.Native_Processes is
   use type C.sys.types.pid_t;

   subtype Command_Type is C.char_ptr_ptr;

   procedure Free (X : in out Command_Type);

   function Image (Command : Command_Type) return String;
   procedure Value (
      Command_Line : String;
      Command : aliased out Command_Type);

   procedure Append (
      Command : aliased in out Command_Type;
      New_Item : String);
   procedure Append (
      Command : aliased in out Command_Type;
      First : Positive;
      Last : Natural);
      --  Copy arguments from argv.

   procedure Append_Argument (
      Command_Line : in out String;
      Last : in out Natural;
      Argument : String);

   --  Child process type

   type Process is record
      Id : C.sys.types.pid_t := -1;
   end record;
   pragma Suppress_Initialization (Process);

   --  Child process management

   function Is_Open (Child : Process) return Boolean;
   pragma Inline (Is_Open);

   Process_Disable_Controlled : constant Boolean := True;

   procedure Create (
      Child : in out Process;
      Command : Command_Type;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Output : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Error : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type);
   procedure Create (
      Child : in out Process;
      Command_Line : String;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Output : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Error : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type);

   procedure Close (Child : in out Process) is null;
   pragma Inline (Close); -- [gcc-7] can not skip calling null procedure

   procedure Wait (
      Child : in out Process;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Wait_Immediate (
      Child : in out Process;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Abort_Process (Child : in out Process);
   procedure Forced_Abort_Process (Child : in out Process);

   --  Pass a command to the shell

   procedure Shell (
      Command : Command_Type;
      Status : out Ada.Command_Line.Exit_Status);
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
