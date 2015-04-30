with Ada.Exception_Identification.From_Here;
with Ada.Streams.Stream_IO.Naked;
with System.Address_To_Named_Access_Conversions;
with System.Environment_Block;
with System.Native_IO;
with System.Synchronous_Control;
with System.Zero_Terminated_Strings;
with C.errno;
with C.spawn;
with C.stdlib;
with C.sys.wait;
with C.unistd;
package body Ada.Processes is
   use Exception_Identification.From_Here;
   use type Command_Line.Exit_Status;
   use type Exception_Identification.Exception_Id;
   use type C.char;
   use type C.char_ptr;
   use type C.signed_int;
   use type C.size_t;

   subtype Arguments_Type is C.char_ptr_array (0 .. 255);

   procedure Split_Argument (
      Command_Line : in out C.char_array;
      Arguments : in out Arguments_Type);
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

   procedure Spawn (
      Child : out C.sys.types.pid_t;
      Command_Line : String;
      Directory : String;
      Search_Path : Boolean;
      Input : System.Native_IO.Handle_Type;
      Output : System.Native_IO.Handle_Type;
      Error : System.Native_IO.Handle_Type);
   procedure Spawn (
      Child : out C.sys.types.pid_t;
      Command_Line : String;
      Directory : String;
      Search_Path : Boolean;
      Input : System.Native_IO.Handle_Type;
      Output : System.Native_IO.Handle_Type;
      Error : System.Native_IO.Handle_Type)
   is
      package char_ptr_Conv is
         new System.Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      Old_Directory : C.char_ptr := null;
      Exception_Id : Exception_Identification.Exception_Id :=
         Exception_Identification.Null_Id;
   begin
      --  set current directory
      if Directory /= "" then
         Old_Directory := C.unistd.getcwd (null, 0);
         declare
            C_Directory : C.char_array (
               0 ..
               Directory'Length * System.Zero_Terminated_Strings.Expanding);
         begin
            System.Zero_Terminated_Strings.To_C (
               Directory,
               C_Directory (0)'Access);
            if C.unistd.chdir (C_Directory (0)'Access) < 0 then
               Exception_Id := Name_Error'Identity;
               goto Cleanup;
            end if;
         end;
      end if;
      --  execute
      declare
         C_Command_Line : C.char_array (
            0 ..
            Command_Line'Length * System.Zero_Terminated_Strings.Expanding);
         Arguments : C.char_ptr_array (0 .. 255) := (others => <>);
         Environment_Block : constant C.char_ptr_ptr :=
            System.Environment_Block;
         Actions : aliased C.spawn.posix_spawn_file_actions_t;
         Attrs : aliased C.spawn.posix_spawnattr_t;
         subtype Handle_Index is C.signed_int range 0 .. 2;
         Source_Handles : array (Handle_Index) of C.signed_int;
         New_Child : aliased C.sys.types.pid_t;
         errno : C.signed_int;
      begin
         System.Zero_Terminated_Strings.To_C (
            Command_Line,
            C_Command_Line (0)'Access);
         Split_Argument (C_Command_Line, Arguments);
         if C.spawn.posix_spawn_file_actions_init (Actions'Access) /= 0 then
            Exception_Id := Use_Error'Identity;
            goto Cleanup;
         end if;
         if C.spawn.posix_spawnattr_init (Attrs'Access) /= 0 then
            Exception_Id := Use_Error'Identity;
            goto Cleanup;
         end if;
         Source_Handles (0) := Input;
         Source_Handles (1) := Output;
         Source_Handles (2) := Error;
         for I in Handle_Index loop
            if Source_Handles (I) /= I then
               if C.spawn.posix_spawn_file_actions_adddup2 (
                  Actions'Access,
                  Source_Handles (I),
                  I) /= 0
               then
                  Exception_Id := Use_Error'Identity;
                  goto Cleanup;
               end if;
            end if;
         end loop;
         if Search_Path then
            errno := C.spawn.posix_spawnp (
               New_Child'Access,
               Arguments (0),
               Actions'Access,
               Attrs'Access,
               Arguments (0)'Access,
               Environment_Block);
         else
            errno := C.spawn.posix_spawn (
               New_Child'Access,
               Arguments (0),
               Actions'Access,
               Attrs'Access,
               Arguments (0)'Access,
               Environment_Block);
         end if;
         case errno is
            when 0 => -- success
               Child := New_Child;
            when C.errno.E2BIG => -- too long arguments
               Exception_Id := Constraint_Error'Identity;
            when C.errno.ENOMEM =>
               Exception_Id := Storage_Error'Identity;
            when C.errno.ENOTDIR
               | C.errno.ENAMETOOLONG
               | C.errno.ENOENT =>
               Exception_Id := Name_Error'Identity;
            when others =>
               Exception_Id := System.Native_IO.IO_Exception_Id (Error);
         end case;
      end;
   <<Cleanup>>
      --  restore current directory
      if Old_Directory /= null then
         if C.unistd.chdir (Old_Directory) < 0 then
            if Exception_Id = Exception_Identification.Null_Id then
               Exception_Id := Name_Error'Identity;
            end if;
         end if;
         C.stdlib.free (C.void_ptr (char_ptr_Conv.To_Address (Old_Directory)));
      end if;
      if Exception_Id /= Exception_Identification.Null_Id then
         Raise_Exception (Exception_Id);
      end if;
   end Spawn;

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
      Spawn (
         C.sys.types.pid_t (Child),
         Command_Line,
         Directory,
         Search_Path,
         Streams.Stream_IO.Naked.Handle (Input),
         Streams.Stream_IO.Naked.Handle (Output),
         Streams.Stream_IO.Naked.Handle (Error));
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

   procedure Wait (
      Child : Process;
      Status : out Command_Line.Exit_Status)
   is
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

   procedure Wait (
      Child : Process)
   is
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
      if Code < 0
         or else Code = 127 * 16#100# -- the execution of the shell failed
      then
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

end Ada.Processes;
