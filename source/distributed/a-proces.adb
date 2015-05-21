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
         Streams.Stream_IO.Standard_Files.Standard_Error.all) is
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
      Child : Process)
   is
      Dummy : Command_Line.Exit_Status;
      pragma Unreferenced (Dummy);
   begin
      Wait (Child, Dummy);
   end Wait;

   procedure Shell (
      Command_Line : String)
   is
      Dummy : Ada.Command_Line.Exit_Status;
      pragma Unreferenced (Dummy);
   begin
      Shell (Command_Line, Dummy);
   end Shell;

end Ada.Processes;
