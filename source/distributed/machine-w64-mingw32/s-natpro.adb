with Ada.Exception_Identification.From_Here;
with Ada.Streams.Naked_Stream_IO.Standard_Files;
with System.Native_IO;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.windef;
with C.winerror;
package body System.Native_Processes is
   use Ada.Exception_Identification.From_Here;
   use type Ada.Command_Line.Exit_Status;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.winnt.HANDLE; -- C.void_ptr

   function memchr (
      s : Address;
      c : Integer;
      n : Storage_Elements.Storage_Count)
      return Address
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memchr";

   --  implementation

   function Do_Is_Open (Child : Process) return Boolean is
   begin
      return Reference (Child).all /= C.winbase.INVALID_HANDLE_VALUE;
   end Do_Is_Open;

   procedure Create (
      Child : in out Process;
      Command_Line : String;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : aliased Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Output : aliased Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Error : aliased Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type)
   is
      pragma Unreferenced (Search_Path);
      W_Command_Line : aliased C.winnt.WCHAR_array (
         0 ..
         Command_Line'Length * Zero_Terminated_WStrings.Expanding);
      W_Directory : aliased C.winnt.WCHAR_array (0 .. Directory'Length);
      Directory_Ref : access constant C.winnt.WCHAR;
      Startup_Info : aliased C.winbase.STARTUPINFO;
      Process_Info : aliased C.winbase.PROCESS_INFORMATION;
      Current_Process : constant C.winnt.HANDLE := C.winbase.GetCurrentProcess;
      subtype Handle_Index is Integer range 0 .. 2;
      Source_Files : array (Handle_Index) of
         access constant Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Target_Handles : array (Handle_Index) of C.winnt.HANDLE;
      Duplicated_Handles : array (Handle_Index) of aliased C.winnt.HANDLE;
      R : C.windef.WINBOOL;
   begin
      C.winbase.GetStartupInfo (Startup_Info'Access);
      Startup_Info.dwFlags := C.winbase.STARTF_USESTDHANDLES
         or C.winbase.STARTF_FORCEOFFFEEDBACK;
      Source_Files (0) := Input'Access;
      Source_Files (1) := Output'Access;
      Source_Files (2) := Error'Access;
      for I in Handle_Index loop
         declare
            Source_Handle : constant C.winnt.HANDLE :=
               Ada.Streams.Naked_Stream_IO.Handle (Source_Files (I).all);
         begin
            if Ada.Streams.Naked_Stream_IO.Is_Standard (
               Source_Files (I).all)
            then
               Duplicated_Handles (I) := C.winbase.INVALID_HANDLE_VALUE;
               Target_Handles (I) := Source_Handle;
            else
               if C.winbase.DuplicateHandle (
                  hSourceProcessHandle => Current_Process,
                  hSourceHandle => Source_Handle,
                  hTargetProcessHandle => Current_Process,
                  lpTargetHandle => Duplicated_Handles (I)'Access,
                  dwDesiredAccess => 0,
                  bInheritHandle => 1,
                  dwOptions => C.winnt.DUPLICATE_SAME_ACCESS) = 0
               then
                  Raise_Exception (Use_Error'Identity);
               end if;
               Target_Handles (I) := Duplicated_Handles (I);
            end if;
         end;
      end loop;
      Startup_Info.hStdInput := Target_Handles (0);
      Startup_Info.hStdOutput := Target_Handles (1);
      Startup_Info.hStdError := Target_Handles (2);
      Zero_Terminated_WStrings.To_C (Command_Line, W_Command_Line (0)'Access);
      if Directory'Length > 0 then
         Zero_Terminated_WStrings.To_C (Directory, W_Directory (0)'Access);
         Directory_Ref := W_Directory (0)'Access;
      else
         Directory_Ref := null;
      end if;
      R := C.winbase.CreateProcess (
         lpApplicationName => null,
         lpCommandLine => W_Command_Line (0)'Access,
         lpProcessAttributes => null,
         lpThreadAttributes => null,
         bInheritHandles => 1,
         dwCreationFlags => 0,
         lpEnvironment => C.windef.LPVOID (Null_Address),
         lpCurrentDirectory => Directory_Ref,
         lpStartupInfo => Startup_Info'Access,
         lpProcessInformation => Process_Info'Access);
      for I in Handle_Index loop
         if Duplicated_Handles (I) /= C.winbase.INVALID_HANDLE_VALUE then
            if C.winbase.CloseHandle (Duplicated_Handles (I)) = 0 then
               Raise_Exception (Use_Error'Identity);
            end if;
         end if;
      end loop;
      if R = 0 then
         declare
            Error : constant C.windef.DWORD := C.winbase.GetLastError;
         begin
            case Error is
               when C.winerror.ERROR_FILE_NOT_FOUND
                  | C.winerror.ERROR_PATH_NOT_FOUND
                  | C.winerror.ERROR_INVALID_NAME =>
                  Raise_Exception (Name_Error'Identity);
               when others =>
                  Raise_Exception (Native_IO.IO_Exception_Id (Error));
            end case;
         end;
      else
         if C.winbase.CloseHandle (Process_Info.hThread) = 0 then
            Raise_Exception (Use_Error'Identity);
         end if;
         Reference (Child).all := Process_Info.hProcess;
      end if;
   end Create;

   procedure Do_Wait (
      Child : in out Process;
      Status : out Ada.Command_Line.Exit_Status)
   is
      Handle : C.winnt.HANDLE
         renames Reference (Child).all;
   begin
      if C.winbase.WaitForSingleObject (Handle, C.winbase.INFINITE) /=
         C.winbase.WAIT_OBJECT_0
      then
         Raise_Exception (Use_Error'Identity);
      else
         declare
            Max : constant := C.windef.DWORD'Modulus / 2; -- 16#8000_0000#
            Exit_Code : aliased C.windef.DWORD;
         begin
            if C.winbase.GetExitCodeProcess (Handle, Exit_Code'Access) = 0 then
               Raise_Exception (Use_Error'Identity);
            end if;
            if C.winbase.CloseHandle (Handle) = 0 then
               Raise_Exception (Use_Error'Identity);
            end if;
            Handle := C.winbase.INVALID_HANDLE_VALUE;
            --  status code
            if Exit_Code < Max then
               Status := Ada.Command_Line.Exit_Status (Exit_Code);
            else
               --  terminated by an unhandled exception
               Status := Ada.Command_Line.Exit_Status'Last;
            end if;
         end;
      end if;
   end Do_Wait;

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status)
   is
      --  unimplemented, should use ShellExecute
      P : Process;
   begin
      Create (P, Command_Line,
         Search_Path => True,
         Input => Ada.Streams.Naked_Stream_IO.Standard_Files.Standard_Input,
         Output => Ada.Streams.Naked_Stream_IO.Standard_Files.Standard_Output,
         Error => Ada.Streams.Naked_Stream_IO.Standard_Files.Standard_Error);
      Do_Wait (P, Status);
   end Shell;

   procedure Append_Argument (
      Command_Line : in out String;
      Last : in out Natural;
      Argument : String)
   is
      Argument_Length : constant Natural := Argument'Length;
      Has_Space : Boolean;
   begin
      --  add separator
      if Last >= Command_Line'First then
         if Last >= Command_Line'Last then
            raise Constraint_Error;
         end if;
         Last := Last + 1;
         Command_Line (Last) := ' ';
      end if;
      --  find space in argument
      Has_Space := memchr (
         Argument'Address,
         Character'Pos (' '),
         Storage_Elements.Storage_Offset (Argument_Length)) /= Null_Address;
      --  open
      if Has_Space then
         if Last >= Command_Line'Last then
            raise Constraint_Error;
         end if;
         Last := Last + 1;
         Command_Line (Last) := '"';
      end if;
      --  argument
      if Last + Argument_Length > Command_Line'Last then
         raise Constraint_Error;
      end if;
      Command_Line (Last + 1 .. Last + Argument_Length) := Argument;
      Last := Last + Argument_Length;
      --  close
      if Has_Space then
         if Last >= Command_Line'Last then
            raise Constraint_Error;
         end if;
         Last := Last + 1;
         Command_Line (Last) := '"';
      end if;
   end Append_Argument;

   package body Controlled is

      function Reference (Object : Process)
         return not null access C.winnt.HANDLE is
      begin
         return Object.Handle'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Process) is
      begin
         if Object.Handle /= C.winbase.INVALID_HANDLE_VALUE then
            if C.winbase.CloseHandle (Object.Handle) = 0 then
               null; -- raise Use_Error;
            end if;
         end if;
      end Finalize;

   end Controlled;

end System.Native_Processes;
