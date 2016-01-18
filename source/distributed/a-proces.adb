with Ada.Streams.Stream_IO.Naked;
package body Ada.Processes is

   --  implementation

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
      Status : out Command_Line.Exit_Status)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Child) or else raise Status_Error);
   begin
      Do_Wait (Child, Status);
   end Wait;

   procedure Wait (
      Child : in out Process)
   is
      Dummy : Command_Line.Exit_Status;
   begin
      Wait (Child, Dummy); -- checking the predicate
   end Wait;

   procedure Wait_Immediate (
      Child : in out Process;
      Terminated : out Boolean;
      Status : out Command_Line.Exit_Status)
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
      Dummy : Command_Line.Exit_Status;
   begin
      Wait_Immediate (Child, Terminated, Dummy); -- checking the predicate
   end Wait_Immediate;

   procedure Shell (
      Command_Line : String)
   is
      Dummy : Ada.Command_Line.Exit_Status;
   begin
      Shell (Command_Line, Dummy);
   end Shell;

end Ada.Processes;
