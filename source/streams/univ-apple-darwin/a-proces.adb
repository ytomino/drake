with Ada.Exception_Identification.From_Here;
with Ada.Streams.Stream_IO.Inside;
with Ada.Processes.Inside;
with System.Synchronous_Control;
with System.Zero_Terminated_Strings;
with C.errno;
with C.stdlib;
with C.sys.wait;
package body Ada.Processes is
   use Exception_Identification.From_Here;
   use type Command_Line.Exit_Status;
   use type C.char;
   use type C.char_ptr;
   use type C.signed_int;
   use type C.size_t;

   function WIFEXITED (x : C.signed_int) return Boolean;
   function WIFEXITED (x : C.signed_int) return Boolean is
   begin
      return x mod (8#177# + 1) = 0; -- x & 0177
   end WIFEXITED;

   function WEXITSTATUS (x : C.signed_int) return C.signed_int;
   function WEXITSTATUS (x : C.signed_int) return C.signed_int is
   begin
      return x / 256;
   end WEXITSTATUS;

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
      Inside.Spawn (
         C.sys.types.pid_t (Child),
         Command_Line,
         Directory,
         Search_Path,
         Streams.Stream_IO.Inside.Handle (Input),
         Streams.Stream_IO.Inside.Handle (Output),
         Streams.Stream_IO.Inside.Handle (Error));
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
      return Result : Process := 0 do -- dummy initial value
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

   procedure Wait (Child : Process; Status : out Command_Line.Exit_Status) is
      Result : C.sys.types.pid_t;
      Code : aliased C.signed_int;
   begin
      loop
         System.Synchronous_Control.Unlock_Abort;
         Result := C.sys.wait.waitpid (
            C.sys.types.pid_t (Child),
            Code'Access,
            0);
         System.Synchronous_Control.Lock_Abort; -- raise if aborted
         if Result < 0 then
            if C.errno.errno /= C.errno.EINTR then
               Raise_Exception (Use_Error'Identity);
            end if;
            --  interrupted and the signal is not "abort", then retry
         else
            if WIFEXITED (Code) then
               Status := Command_Line.Exit_Status (WEXITSTATUS (Code));
            else
               Status := -1;
            end if;
            exit;
         end if;
      end loop;
   end Wait;

   procedure Wait (Child : Process) is
      Dummy : Command_Line.Exit_Status;
      pragma Unreferenced (Dummy);
   begin
      Wait (Child, Dummy);
   end Wait;

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status)
   is
      C_Command_Line : C.char_array (
         0 ..
         Command_Line'Length * System.Zero_Terminated_Strings.Expanding);
      Code : C.signed_int;
   begin
      System.Zero_Terminated_Strings.To_C (
         Command_Line,
         C_Command_Line (0)'Access);
      Code := C.stdlib.C_system (C_Command_Line (0)'Access);
      if Code = -1 then
         Raise_Exception (Name_Error'Identity);
      else
         if WIFEXITED (Code) then
            Status := Ada.Command_Line.Exit_Status (WEXITSTATUS (Code));
         else
            Status := -1;
         end if;
      end if;
   end Shell;

   procedure Shell (
      Command_Line : String)
   is
      Dummy : Ada.Command_Line.Exit_Status;
      pragma Unreferenced (Dummy);
   begin
      Shell (Command_Line, Dummy);
   end Shell;

   procedure Append_Argument (
      Command_Line : in out String;
      Last : in out Natural;
      Argument : String) is
   begin
      if Last >= Command_Line'First then
         if Last >= Command_Line'Last then
            raise Constraint_Error;
         end if;
         Last := Last + 1;
         Command_Line (Last) := ' ';
      end if;
      for I in Argument'Range loop
         if Argument (I) = ' ' then
            if Last + 1 >= Command_Line'Last then
               raise Constraint_Error;
            end if;
            Last := Last + 1;
            Command_Line (Last) := '\';
            Last := Last + 1;
            Command_Line (Last) := ' ';
         else
            if Last >= Command_Line'Last then
               raise Constraint_Error;
            end if;
            Last := Last + 1;
            Command_Line (Last) := Argument (I);
         end if;
      end loop;
   end Append_Argument;

   procedure Split_Argument (
      Command_Line : in out C.char_array;
      Arguments : in out Arguments_Type)
   is
      I : C.size_t := Command_Line'First;
      Argument_Index : C.size_t := Arguments'First;
   begin
      Arguments (Argument_Index) := null;
      loop
         --  skip spaces
         while Command_Line (I) = ' ' loop
            I := I + 1;
         end loop;
         exit when Command_Line (I) = C.char'Val (0);
         --  take on argument
         Arguments (Argument_Index) := Command_Line (I)'Unchecked_Access;
         declare
            J : C.size_t := I;
         begin
            loop
               if Command_Line (I) = '\' then
                  I := I + 1;
                  exit when Command_Line (I) = C.char'Val (0);
               end if;
               Command_Line (J) := Command_Line (I);
               I := I + 1;
               J := J + 1;
               if Command_Line (I) = ' ' then
                  I := I + 1;
                  exit;
               end if;
               exit when Command_Line (I) = C.char'Val (0);
            end loop;
            Command_Line (J) := C.char'Val (0);
         end;
         Argument_Index := Argument_Index + 1;
         if Argument_Index > Arguments'Last then
            raise Constraint_Error;
         end if;
      end loop;
      Arguments (Argument_Index) := null;
   end Split_Argument;

end Ada.Processes;
