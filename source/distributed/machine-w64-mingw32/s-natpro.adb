with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with Ada.Streams.Naked_Stream_IO.Standard_Files;
with System.Address_To_Named_Access_Conversions;
with System.Native_IO;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Wide_Startup; -- that is also linked by Ada.Command_Line
with System.Zero_Terminated_WStrings;
with System.Debug; -- assertions
with C.string;
with C.windef;
with C.winerror;
package body System.Native_Processes is
   use Ada.Exception_Identification.From_Here;
   use type Ada.Command_Line.Exit_Status;
   use type Storage_Elements.Storage_Offset;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.windef.UINT;
   use type C.winnt.HANDLE; -- C.void_ptr
   use type C.winnt.LPWSTR; -- Command_Type

   function memchr (
      s : Address;
      c : Integer;
      n : Storage_Elements.Storage_Count)
      return Address
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memchr";

   package LPWSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.WCHAR, C.winnt.LPWSTR);

   package LPWSTR_ptr_Conv is
      new Address_To_Named_Access_Conversions (
         C.winnt.LPWSTR,
         C.winnt.LPWSTR_ptr);

   function "+" (Left : C.winnt.LPWSTR_ptr; Right : C.ptrdiff_t)
      return C.winnt.LPWSTR_ptr
      with Convention => Intrinsic;
   pragma Inline_Always ("+");

   function "+" (Left : C.winnt.LPWSTR_ptr; Right : C.ptrdiff_t)
      return C.winnt.LPWSTR_ptr is
   begin
      return LPWSTR_ptr_Conv.To_Pointer (
         LPWSTR_ptr_Conv.To_Address (Left)
            + Storage_Elements.Storage_Offset (Right)
               * (C.winnt.LPWSTR'Size / Standard'Storage_Unit));
   end "+";

   procedure Reallocate (X : in out Command_Type; Length : C.size_t);
   procedure Reallocate (X : in out Command_Type; Length : C.size_t) is
      Size : constant Storage_Elements.Storage_Count :=
         (Storage_Elements.Storage_Offset (Length) + 1) -- NUL
         * (C.winnt.WCHAR'Size / Standard'Storage_Unit);
   begin
      X := LPWSTR_Conv.To_Pointer (
         Standard_Allocators.Reallocate (LPWSTR_Conv.To_Address (X), Size));
   end Reallocate;

   procedure C_Append (
      Command : Command_Type;
      Index : in out C.size_t;
      New_Item : not null C.winnt.LPWSTR);
   procedure C_Append (
      Command : Command_Type;
      Index : in out C.size_t;
      New_Item : not null C.winnt.LPWSTR)
   is
      New_Item_Length : constant C.size_t := C.string.wcslen (New_Item);
      Command_All : C.winnt.WCHAR_array (
         0 .. Index + New_Item_Length + 3); -- ' ' & '"' & '"' & NUL
      for Command_All'Address use LPWSTR_Conv.To_Address (Command);
   begin
      if Index > 0 then
         Command_All (Index) := C.winnt.WCHAR'Val (Character'Pos (' '));
         Index := Index + 1;
      end if;
      declare
         Has_Space : constant Boolean :=
            C.string.wcschr (
               New_Item,
               C.wchar_t'Val (Character'Pos (' '))) /= null;
      begin
         if Has_Space then
            Command_All (Index) := C.winnt.WCHAR'Val (Character'Pos ('"'));
            Index := Index + 1;
         end if;
         if New_Item_Length > 0 then
            declare
               New_Item_All : C.winnt.WCHAR_array (0 .. New_Item_Length - 1);
               for New_Item_All'Address use LPWSTR_Conv.To_Address (New_Item);
            begin
               Command_All (Index .. Index + New_Item_Length - 1) :=
                  New_Item_All;
               Index := Index + New_Item_Length;
            end;
         end if;
         if Has_Space then
            Command_All (Index) := C.winnt.WCHAR'Val (Character'Pos ('"'));
            Index := Index + 1;
         end if;
      end;
      Command_All (Index) := C.winnt.WCHAR'Val (0);
   end C_Append;

   --  implementation

   procedure Free (X : in out Command_Type) is
   begin
      Standard_Allocators.Free (LPWSTR_Conv.To_Address (X));
      X := null;
   end Free;

   function Image (Command : Command_Type) return String is
   begin
      return Zero_Terminated_WStrings.Value (Command);
   end Image;

   procedure Value (
      Command_Line : String;
      Command : aliased out Command_Type)
   is
      Size : constant Storage_Elements.Storage_Count :=
         (Command_Line'Length * Zero_Terminated_WStrings.Expanding + 1)
         * (C.winnt.WCHAR'Size / Standard'Storage_Unit);
   begin
      Command := LPWSTR_Conv.To_Pointer (Standard_Allocators.Allocate (Size));
      Zero_Terminated_WStrings.To_C (Command_Line, Command);
   end Value;

   procedure Append (
      Command : aliased in out Command_Type;
      New_Item : String)
   is
      W_New_Item : aliased C.winnt.WCHAR_array (
         0 .. New_Item'Length * Zero_Terminated_WStrings.Expanding);
      W_New_Item_Length : C.size_t;
      Old_Length : C.size_t;
   begin
      Zero_Terminated_WStrings.To_C (
         New_Item,
         W_New_Item (0)'Access,
         W_New_Item_Length);
      if Command = null then
         Old_Length := 0;
      else
         Old_Length := C.string.wcslen (Command);
      end if;
      Reallocate (
         Command,
         Old_Length + W_New_Item_Length + 3); -- space and a pair of '"'
      C_Append (Command, Old_Length, W_New_Item (0)'Unchecked_Access);
   end Append;

   procedure Append (
      Command : aliased in out Command_Type;
      First : Positive;
      Last : Natural)
   is
      pragma Assert (Last >= First);
      Old_Length : C.size_t;
      Additional_Length : C.size_t := 0;
   begin
      --  get length
      for I in First .. Last loop
         declare
            P : constant C.winnt.LPWSTR_ptr :=
               LPWSTR_ptr_Conv.To_Pointer (Wide_Startup.wargv)
               + C.ptrdiff_t (I);
         begin
            Additional_Length :=
               Additional_Length
               + C.string.wcslen (P.all)
               + 3; -- space and a pair of '"'
         end;
      end loop;
      if Command = null then
         Old_Length := 0;
      else
         Old_Length := C.string.wcslen (Command);
      end if;
      Reallocate (Command, Old_Length + Additional_Length);
      --  copy
      declare
         Index : C.size_t := Old_Length;
      begin
         for I in First .. Last loop
            declare
               P : constant C.winnt.LPWSTR_ptr :=
                  LPWSTR_ptr_Conv.To_Pointer (Wide_Startup.wargv)
                  + C.ptrdiff_t (I);
            begin
               C_Append (Command, Index, P.all);
            end;
         end loop;
      end;
   end Append;

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

   --  child process management

   procedure Wait (
      Child : in out Process;
      Milliseconds : C.windef.DWORD;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status);
   procedure Wait (
      Child : in out Process;
      Milliseconds : C.windef.DWORD;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status) is
   begin
      case C.winbase.WaitForSingleObject (Child.Handle, Milliseconds) is
         when C.winbase.WAIT_OBJECT_0 =>
            declare
               Max : constant := C.windef.DWORD'Modulus / 2; -- 2 ** 31
               Exit_Code : aliased C.windef.DWORD;
               Success : C.windef.WINBOOL;
            begin
               Success :=
                  C.winbase.GetExitCodeProcess (
                     Child.Handle,
                     Exit_Code'Access);
               if C.winbase.CloseHandle (Child.Handle) = C.windef.FALSE
                  or else Success = C.windef.FALSE
               then
                  Raise_Exception (Use_Error'Identity);
               end if;
               Child := Null_Process;
               --  status code
               if Exit_Code < Max then
                  Status := Ada.Command_Line.Exit_Status (Exit_Code);
               else
                  --  terminated by an unhandled exception
                  Status := -1;
               end if;
               Terminated := True;
            end;
         when C.winerror.WAIT_TIMEOUT =>
            Terminated := False;
         when others =>
            Raise_Exception (Use_Error'Identity);
      end case;
   end Wait;

   --  implementation of child process management

   function Is_Open (Child : Process) return Boolean is
   begin
      return Child.Handle /= C.winbase.INVALID_HANDLE_VALUE;
   end Is_Open;

   procedure Create (
      Child : in out Process;
      Command : Command_Type;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Output : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Error : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type)
   is
      pragma Unreferenced (Search_Path);
      W_Directory : aliased C.winnt.WCHAR_array (0 .. Directory'Length);
      Directory_Ref : access constant C.winnt.WCHAR;
      Startup_Info : aliased C.winbase.STARTUPINFO;
      Process_Info : aliased C.winbase.PROCESS_INFORMATION;
      Current_Process : constant C.winnt.HANDLE := C.winbase.GetCurrentProcess;
      subtype Handle_Index is Integer range 0 .. 2;
      Source_Files :
         array (Handle_Index) of
            Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Target_Handles : array (Handle_Index) of C.winnt.HANDLE;
      Duplicated_Handles : array (Handle_Index) of aliased C.winnt.HANDLE;
      Success : C.windef.WINBOOL;
   begin
      C.winbase.GetStartupInfo (Startup_Info'Access);
      Startup_Info.dwFlags := C.winbase.STARTF_USESTDHANDLES
         or C.winbase.STARTF_FORCEOFFFEEDBACK;
      Source_Files (0) := Input;
      Source_Files (1) := Output;
      Source_Files (2) := Error;
      for I in Handle_Index loop
         declare
            Source_Handle : constant C.winnt.HANDLE :=
               Ada.Streams.Naked_Stream_IO.Handle (Source_Files (I));
         begin
            if Ada.Streams.Naked_Stream_IO.Is_Standard (Source_Files (I)) then
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
                     dwOptions => C.winnt.DUPLICATE_SAME_ACCESS) =
                  C.windef.FALSE
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
      if Directory'Length > 0 then
         Zero_Terminated_WStrings.To_C (Directory, W_Directory (0)'Access);
         Directory_Ref := W_Directory (0)'Access;
      else
         Directory_Ref := null;
      end if;
      Success := C.winbase.CreateProcess (
         lpApplicationName => null,
         lpCommandLine => Command,
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
            if C.winbase.CloseHandle (Duplicated_Handles (I)) =
               C.windef.FALSE
            then
               Raise_Exception (Use_Error'Identity);
            end if;
         end if;
      end loop;
      if Success = C.windef.FALSE then
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
         if C.winbase.CloseHandle (Process_Info.hThread) = C.windef.FALSE then
            Raise_Exception (Use_Error'Identity);
         end if;
         Child.Handle := Process_Info.hProcess;
      end if;
   end Create;

   procedure Create (
      Child : in out Process;
      Command_Line : String;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Output : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Error : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type)
   is
      W_Command_Line : aliased C.winnt.WCHAR_array (
         0 ..
         Command_Line'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Command_Line, W_Command_Line (0)'Access);
      Create (
         Child,
         W_Command_Line (0)'Unchecked_Access,
         Directory,
         Search_Path,
         Input,
         Output,
         Error);
   end Create;

   procedure Close (Child : in out Process) is
   begin
      if Is_Open (Child) then
         declare
            Success : C.windef.WINBOOL;
         begin
            Success := C.winbase.CloseHandle (Child.Handle);
            pragma Check (Debug,
               Check =>
                  Success /= C.windef.FALSE
                  or else Debug.Runtime_Error ("CloseHandle failed"));
         end;
      end if;
   end Close;

   procedure Wait (
      Child : in out Process;
      Status : out Ada.Command_Line.Exit_Status) is
   begin
      loop
         declare
            Terminated : Boolean;
         begin
            Wait (Child, C.winbase.INFINITE,
               Terminated => Terminated, Status => Status);
            exit when Terminated;
         end;
      end loop;
   end Wait;

   procedure Wait_Immediate (
      Child : in out Process;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status) is
   begin
      Wait (Child, 0, Terminated => Terminated, Status => Status);
   end Wait_Immediate;

   procedure Forced_Abort_Process (Child : Process) is
      Code : constant C.windef.UINT := -1; -- the MSB should be 1 ???
   begin
      if C.winbase.TerminateProcess (Child.Handle, Code) = C.windef.FALSE then
         declare
            Exit_Code : aliased C.windef.DWORD;
         begin
            --  It is not an error if the process is already terminated.
            if not (
               C.winbase.GetLastError = C.winerror.ERROR_ACCESS_DENIED
               and then C.winbase.GetExitCodeProcess (
                     Child.Handle,
                     Exit_Code'Access) /=
                  C.windef.FALSE
               and then Exit_Code /= C.winbase.STILL_ACTIVE)
            then
               Raise_Exception (Use_Error'Identity);
            end if;
         end;
      end if;
   end Forced_Abort_Process;

   --  implementation of pass a command to the shell

   procedure Shell (
      Command : Command_Type;
      Status : out Ada.Command_Line.Exit_Status)
   is
      --  unimplemented, should use ShellExecute
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (Process, Close);
      P : aliased Process := Null_Process;
   begin
      Holder.Assign (P);
      Create (P, Command,
         Search_Path => True,
         Input => Ada.Streams.Naked_Stream_IO.Standard_Files.Standard_Input,
         Output => Ada.Streams.Naked_Stream_IO.Standard_Files.Standard_Output,
         Error => Ada.Streams.Naked_Stream_IO.Standard_Files.Standard_Error);
      Wait (P, Status);
   end Shell;

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status)
   is
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (Command_Type, Free);
      Command : aliased Command_Type;
   begin
      Holder.Assign (Command);
      Value (Command_Line, Command);
      Shell (Command, Status);
   end Shell;

end System.Native_Processes;
