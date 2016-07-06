with Ada.Streams.Stream_IO.Naked;
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
      Native_Command : System.Native_Processes.Command_Type
         renames Controlled_Commands.Reference (Command).all;
   begin
      Create (
         Child,
         Native_Command,
         Directory,
         Search_Path,
         Streams.Stream_IO.Naked.Non_Controlled (Input).all,
         Streams.Stream_IO.Naked.Non_Controlled (Output).all,
         Streams.Stream_IO.Naked.Non_Controlled (Error).all);
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
   begin
      Create (
         Child,
         Command_Line,
         Directory,
         Search_Path,
         Streams.Stream_IO.Naked.Non_Controlled (Input).all,
         Streams.Stream_IO.Naked.Non_Controlled (Output).all,
         Streams.Stream_IO.Naked.Non_Controlled (Error).all);
   end Create;

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
      return Process is
   begin
      return Result : Process do
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
      return Process is
   begin
      return Result : Process do
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

   procedure Wait (
      Child : in out Process;
      Status : out Ada.Command_Line.Exit_Status)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Child) or else raise Status_Error);
   begin
      Do_Wait (Child, Status);
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
   begin
      Do_Wait_Immediate (Child, Terminated, Status);
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
   begin
      Do_Abort_Process (Child);
   end Abort_Process;

   procedure Forced_Abort_Process (Child : in out Process) is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Child) or else raise Status_Error);
   begin
      Do_Forced_Abort_Process (Child);
   end Forced_Abort_Process;

   procedure Shell (
      Command : Command_Type;
      Status : out Ada.Command_Line.Exit_Status)
   is
      Native_Command : System.Native_Processes.Command_Type
         renames Controlled_Commands.Reference (Command).all;
   begin
      System.Native_Processes.Shell (Native_Command, Status);
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

end Ada.Processes;
