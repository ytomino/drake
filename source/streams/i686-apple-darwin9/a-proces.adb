with Ada.Environment_Variables.Inside;
with Ada.Streams.Stream_IO.Inside;
with C.stdlib;
with C.unistd;
with C.sys.fcntl;
with C.sys.wait;
package body Ada.Processes is
   use type Command_Line.Exit_Status;
   use type C.char_ptr;
   use type C.signed_int;
   use type C.size_t;

   function WIFEXITED (x : C.signed_int) return Boolean;
   function WIFEXITED (x : C.signed_int) return Boolean is
   begin
      return x mod (8#177# + 1) = 0; --  x & 0177
   end WIFEXITED;

   function WEXITSTATUS (x : C.signed_int) return C.signed_int;
   function WEXITSTATUS (x : C.signed_int) return C.signed_int is
   begin
      return x / 256;
   end WEXITSTATUS;

   procedure Create (
      Child : in out Process;
      Command_Line : String;
      Directory : String := "";
      Input : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Input.all;
      Output : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Output.all;
      Error : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Error.all)
   is
      Result : C.sys.types.pid_t;
   begin
      Result := C.unistd.vfork;
      if Result < 0 then
         raise Use_Error;
      elsif Result > 0 then
         --  parent process
         Child := Process (Result);
      else
         --  child process
         if Directory /= "" then
            declare
               Z_Directory : String := Directory & Character'Val (0);
               C_Directory : C.char_array (0 .. Directory'Length);
               for C_Directory'Address use Z_Directory'Address;
            begin
               if C.unistd.chdir (C_Directory (0)'Access) = -1 then
                  C.unistd.C_exit (127);
               end if;
            end;
         end if;
         declare
            Parsed : C.char_array (0 .. Command_Line'Length);
            Argument : array (C.size_t range 0 .. 255) of aliased C.char_ptr;
            I : Integer := Command_Line'First;
            J : C.size_t := Parsed'First;
            Argument_Index : C.size_t := Argument'First;
            Duplicated_Input : Streams.Stream_IO.Inside.Handle_Type;
            Duplicated_Output : Streams.Stream_IO.Inside.Handle_Type;
            Duplicated_Error : Streams.Stream_IO.Inside.Handle_Type;
            Dummy : C.signed_int;
            pragma Unreferenced (Dummy);
         begin
            Argument (Argument_Index) := null;
            Parsing : while I <= Command_Line'Last loop
               if Command_Line (I) = ' ' then
                  Parsed (J) := C.char'Val (0);
                  J := J + 1;
                  loop
                     I := I + 1;
                     exit Parsing when I > Command_Line'Last;
                     exit when Command_Line (I) /= ' ';
                  end loop;
                  if Argument (Argument_Index) /= null then
                     Argument_Index := Argument_Index + 1;
                     Argument (Argument_Index) := null;
                  end if;
               else
                  if Argument (Argument_Index) = null then
                     Argument (Argument_Index) := Parsed (J)'Unchecked_Access;
                  end if;
                  if Command_Line (I) = '\' then
                     I := I + 1;
                  end if;
                  Parsed (J) := C.char (Command_Line (I));
                  I := I + 1;
                  J := J + 1;
               end if;
            end loop Parsing;
            Parsed (J) := C.char'Val (0);
            Argument_Index := Argument_Index + 1;
            Argument (Argument_Index) := null;
            --  duplicate handles
            Duplicated_Input := C.unistd.dup (
               Streams.Stream_IO.Inside.Handle (Input));
            Duplicated_Output := C.unistd.dup (
               Streams.Stream_IO.Inside.Handle (Output));
            Duplicated_Error := C.unistd.dup (
               Streams.Stream_IO.Inside.Handle (Error));
            --  close standard handles
            Dummy := C.unistd.close (0);
            Dummy := C.unistd.close (1);
            Dummy := C.unistd.close (2);
            --  set standard handles
            Dummy := C.unistd.dup2 (Duplicated_Input, 0);
            Dummy := C.unistd.dup2 (Duplicated_Output, 1);
            Dummy := C.unistd.dup2 (Duplicated_Error, 2);
            --  close duplicated handles
            Dummy := C.unistd.close (Duplicated_Input);
            Dummy := C.unistd.close (Duplicated_Output);
            Dummy := C.unistd.close (Duplicated_Error);
            --  clear FD_CLOEXEC
            Dummy := C.sys.fcntl.fcntl (0, C.sys.fcntl.F_SETFD, 0);
            Dummy := C.sys.fcntl.fcntl (1, C.sys.fcntl.F_SETFD, 0);
            Dummy := C.sys.fcntl.fcntl (2, C.sys.fcntl.F_SETFD, 0);
            Dummy := C.unistd.execve (
               Argument (0),
               Argument (1)'Access,
               Environment_Variables.Inside.Environment_Block);
            C.unistd.C_exit (127);
         end;
      end if;
   end Create;

   function Create (
      Command_Line : String;
      Directory : String := "";
      Input : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Input.all;
      Output : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Output.all;
      Error : Streams.Stream_IO.File_Type :=
         Streams.Stream_IO.Standards.Standard_Error.all)
      return Process is
   begin
      return Result : Process := 0 do -- dummy initial value
         Create (Result, Command_Line, Directory, Input, Output, Error);
      end return;
   end Create;

   procedure Wait (Child : Process; Status : out Command_Line.Exit_Status) is
      Result : C.sys.types.pid_t;
      Code : aliased C.signed_int;
   begin
      Result := C.sys.wait.waitpid (
         C.sys.types.pid_t (Child),
         Code'Access,
         0);
      if Result < 0 then
         raise Use_Error;
      else
         if WIFEXITED (Code) then
            Status := Command_Line.Exit_Status (WEXITSTATUS (Code));
         else
            Status := -1;
         end if;
      end if;
   end Wait;

   procedure Wait (Child : Process) is
      Dummy : Command_Line.Exit_Status;
      pragma Unreferenced (Dummy);
   begin
      Wait (Child, Dummy);
   end Wait;

   procedure Shell (Command : String; Status : out Command_Line.Exit_Status) is
      Z_Command : String := Command & Character'Val (0);
      C_Command : C.char_array (0 .. Z_Command'Length);
      for C_Command'Address use Z_Command'Address;
      Code : C.signed_int;
   begin
      Code := C.stdlib.C_system (C_Command (0)'Access);
      if Code = -1 then
         raise Name_Error;
      else
         if WIFEXITED (Code) then
            Status := Command_Line.Exit_Status (WEXITSTATUS (Code));
         else
            Status := -1;
         end if;
      end if;
   end Shell;

   procedure Shell (Command : String) is
      Dummy : Command_Line.Exit_Status;
      pragma Unreferenced (Dummy);
   begin
      Shell (Command, Dummy);
   end Shell;

end Ada.Processes;
