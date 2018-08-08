pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Naked_Stream_IO;
with C.winbase;
with C.winnt;
package System.Native_Processes is

   subtype Command_Type is C.winnt.LPWSTR;

   procedure Free (X : in out Command_Type);

   function Image (Command : Command_Type) return String;
   procedure Value (
      Command_Line : String;
      Command : aliased out Command_Type);

   pragma Inline (Image);

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
      Handle : C.winnt.HANDLE;
   end record;
      --  [gcc-7.3] crashes if "type Process is new C.winnt.HANDLE;"
   pragma Suppress_Initialization (Process);

   Null_Process : constant Process :=
      (Handle => C.winbase.INVALID_HANDLE_VALUE);

   --  Child process management

   function Is_Open (Child : Process) return Boolean;
   pragma Inline (Is_Open);

   Process_Disable_Controlled : constant Boolean := False;

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

   procedure Close (Child : in out Process);

   procedure Wait (
      Child : in out Process;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Wait_Immediate (
      Child : in out Process;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Forced_Abort_Process (Child : Process);

   procedure Abort_Process (Child : Process)
      renames Forced_Abort_Process;
      --  Should it use CREATE_NEW_PROCESS_GROUP and GenerateConsoleCtrlEvent?

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
