pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Naked_Stream_IO;
with C.winbase;
with C.winnt;
private with Ada.Finalization;
package System.Native_Processes is

   procedure Append_Argument (
      Command_Line : in out String;
      Last : in out Natural;
      Argument : String);

   --  Child process type

   type Process is limited private;

   --  Child process management

   function Do_Is_Open (Child : Process) return Boolean;
   pragma Inline (Do_Is_Open);

   procedure Create (
      Child : in out Process;
      Command_Line : String;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : aliased Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Output : aliased Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Error : aliased Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type);

   procedure Do_Wait (
      Child : in out Process;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Do_Wait_Immediate (
      Child : in out Process;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Do_Forced_Abort_Process (Child : in out Process);

   procedure Do_Abort_Process (Child : in out Process)
      renames Do_Forced_Abort_Process;
      --  Should it use CREATE_NEW_PROCESS_GROUP and GenerateConsoleCtrlEvent?

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

private

   package Controlled is

      type Process is limited private;

      function Reference (Object : Native_Processes.Process)
         return not null access C.winnt.HANDLE;
      pragma Inline (Reference);

   private

      type Process is
         limited new Ada.Finalization.Limited_Controlled with
      record
         Handle : aliased C.winnt.HANDLE := C.winbase.INVALID_HANDLE_VALUE;
      end record;

      overriding procedure Finalize (Object : in out Process);

   end Controlled;

   type Process is new Controlled.Process;

end System.Native_Processes;
