with Ada.Streams.Naked_Stream_IO;
with Ada.Streams.Stream_IO.Naked;
with System.Unwind.Occurrences;
package body Ada.Processes is

   --  implementation

   function Image (Command : Command_Type) return String is
      Native_Command : System.Native_Processes.Command_Type
         renames Controlled_Commands.Reference (Command).all;
   begin
      return System.Native_Processes.Image (Native_Command);
   end Image;

   function Value (Command_Line : String) return Command_Type is
   begin
      return Result : Command_Type do
         declare
            Native_Result : System.Native_Processes.Command_Type
               renames Controlled_Commands.Reference (Result).all;
         begin
            System.Native_Processes.Value (Command_Line, Native_Result);
         end;
      end return;
   end Value;

   procedure Append (Command : in out Command_Type; New_Item : String) is
      Native_Command : System.Native_Processes.Command_Type
         renames Controlled_Commands.Reference (Command).all;
   begin
      System.Native_Processes.Append (Native_Command, New_Item);
   end Append;

   procedure Append (
      Command : in out Command_Type;
      New_Item :
         Ada.Command_Line.Iterator_Interfaces.Reversible_Iterator'Class)
   is
      Native_Command : System.Native_Processes.Command_Type
         renames Controlled_Commands.Reference (Command).all;
      First : constant Natural :=
         Ada.Command_Line.Iterator_Interfaces.First (New_Item);
   begin
      if Ada.Command_Line.Has_Element (First) then
         System.Native_Processes.Append (Native_Command,
            First => First,
            Last => Ada.Command_Line.Iterator_Interfaces.Last (New_Item));
      end if;
   end Append;

   function Is_Open (Child : Process) return Boolean is
      N_Child : System.Native_Processes.Process
         renames Controlled_Processes.Reference (Child).all;
   begin
      return System.Native_Processes.Is_Open (N_Child);
   end Is_Open;

   procedure Create (
      Child : in out Process;
      Command : Command_Type;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standard_Files.Standard_Input.all;
      Output : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standard_Files.Standard_Output.all;
      Error : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standard_Files.Standard_Error.all)
   is
      pragma Check (Pre, not Is_Open (Child) or else raise Status_Error);
      Naked_Output : constant
            not null access Streams.Naked_Stream_IO.Non_Controlled_File_Type :=
         Streams.Stream_IO.Naked.Non_Controlled (Output);
      Naked_Error : constant
            not null access Streams.Naked_Stream_IO.Non_Controlled_File_Type :=
         Streams.Stream_IO.Naked.Non_Controlled (Error);
   begin
      if Streams.Naked_Stream_IO.Is_Standard (Naked_Output.all)
         or else Streams.Naked_Stream_IO.Is_Standard (Naked_Error.all)
      then
         System.Unwind.Occurrences.Flush_IO;
      end if;
      declare
         N_Child : System.Native_Processes.Process
            renames Controlled_Processes.Reference (Child).all;
         Native_Command : System.Native_Processes.Command_Type
            renames Controlled_Commands.Reference (Command).all;
      begin
         System.Native_Processes.Create (
            N_Child,
            Native_Command,
            Directory,
            Search_Path,
            Streams.Stream_IO.Naked.Non_Controlled (Input).all,
            Naked_Output.all,
            Naked_Error.all);
      end;
   end Create;

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
         Streams.Stream_IO.Standard_Files.Standard_Error.all)
   is
      pragma Check (Pre, not Is_Open (Child) or else raise Status_Error);
      N_Child : System.Native_Processes.Process
         renames Controlled_Processes.Reference (Child).all;
   begin
      System.Native_Processes.Create (
         N_Child,
         Command_Line,
         Directory,
         Search_Path,
         Streams.Stream_IO.Naked.Non_Controlled (Input).all,
         Streams.Stream_IO.Naked.Non_Controlled (Output).all,
         Streams.Stream_IO.Naked.Non_Controlled (Error).all);
   end Create;

   procedure Wait (
      Child : in out Process;
      Status : out Ada.Command_Line.Exit_Status)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Child) or else raise Status_Error);
      N_Child : System.Native_Processes.Process
         renames Controlled_Processes.Reference (Child).all;
   begin
      System.Native_Processes.Wait (N_Child, Status);
   end Wait;

   procedure Wait (
      Child : in out Process)
   is
      Dummy : Ada.Command_Line.Exit_Status;
   begin
      Wait (Child, Dummy); -- checking the predicate
   end Wait;

   procedure Wait_Immediate (
      Child : in out Process;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Child) or else raise Status_Error);
      N_Child : System.Native_Processes.Process
         renames Controlled_Processes.Reference (Child).all;
   begin
      System.Native_Processes.Wait_Immediate (N_Child, Terminated, Status);
   end Wait_Immediate;

   procedure Wait_Immediate (
      Child : in out Process;
      Terminated : out Boolean)
   is
      Dummy : Ada.Command_Line.Exit_Status;
   begin
      Wait_Immediate (Child, Terminated, Dummy); -- checking the predicate
   end Wait_Immediate;

   procedure Abort_Process (Child : in out Process) is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Child) or else raise Status_Error);
      N_Child : System.Native_Processes.Process
         renames Controlled_Processes.Reference (Child).all;
   begin
      System.Native_Processes.Abort_Process (N_Child);
   end Abort_Process;

   procedure Forced_Abort_Process (Child : in out Process) is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Child) or else raise Status_Error);
      N_Child : System.Native_Processes.Process
         renames Controlled_Processes.Reference (Child).all;
   begin
      System.Native_Processes.Forced_Abort_Process (N_Child);
   end Forced_Abort_Process;

   procedure Shell (
      Command : Command_Type;
      Status : out Ada.Command_Line.Exit_Status) is
   begin
      System.Unwind.Occurrences.Flush_IO;
      declare
         Native_Command : System.Native_Processes.Command_Type
            renames Controlled_Commands.Reference (Command).all;
      begin
         System.Native_Processes.Shell (Native_Command, Status);
      end;
   end Shell;

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status) is
   begin
      System.Unwind.Occurrences.Flush_IO;
      System.Native_Processes.Shell (Command_Line, Status);
   end Shell;

   procedure Shell (Command : Command_Type) is
      Dummy : Ada.Command_Line.Exit_Status;
   begin
      Shell (Command, Dummy);
   end Shell;

   procedure Shell (Command_Line : String) is
      Dummy : Ada.Command_Line.Exit_Status;
   begin
      Shell (Command_Line, Dummy);
   end Shell;

   package body Controlled_Commands is

      function Reference (Object : Processes.Command_Type)
         return not null access System.Native_Processes.Command_Type is
      begin
         return Command_Type (Object).Native_Command'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Command_Type) is
      begin
         System.Native_Processes.Free (Object.Native_Command);
      end Finalize;

   end Controlled_Commands;

   package body Controlled_Processes is

      function Reference (Object : Processes.Process)
         return not null access System.Native_Processes.Process is
      begin
         return Process (Object).Data'Unrestricted_Access;
      end Reference;

      function Create (
         Command : Command_Type;
         Directory : String := "";
         Search_Path : Boolean := False;
         Input : Streams.Stream_IO.File_Type :=
            Streams.Stream_IO.Standard_Files.Standard_Input.all;
         Output : Streams.Stream_IO.File_Type :=
            Streams.Stream_IO.Standard_Files.Standard_Output.all;
         Error : Streams.Stream_IO.File_Type :=
            Streams.Stream_IO.Standard_Files.Standard_Error.all)
         return Processes.Process is
      begin
         return Result : Processes.Process do
            Create (
               Result,
               Command,
               Directory,
               Search_Path,
               Input,
               Output,
               Error);
         end return;
      end Create;

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
         return Processes.Process is
      begin
         return Result : Processes.Process do
            Create (
               Result,
               Command_Line,
               Directory,
               Search_Path,
               Input,
               Output,
               Error);
         end return;
      end Create;

      overriding procedure Finalize (Object : in out Process) is
      begin
         System.Native_Processes.Close (Object.Data);
      end Finalize;

   end Controlled_Processes;

end Ada.Processes;
