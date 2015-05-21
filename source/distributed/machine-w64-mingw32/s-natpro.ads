pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Naked_Stream_IO;
with C.winbase;
with C.winnt;
private with Ada.Finalization;
package System.Native_Processes is

   type Process is limited private;

   procedure Create (
      Child : in out Process;
      Command_Line : String;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : aliased Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Output : aliased Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Error : aliased Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type);

   procedure Do_Wait (
      Child : Process;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status);

   procedure Append_Argument (
      Command_Line : in out String;
      Last : in out Natural;
      Argument : String);

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception
      renames Ada.IO_Exceptions.Device_Error;

private

   package Controlled is

      type Process is limited private;

      function Reference (Object : Process)
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
