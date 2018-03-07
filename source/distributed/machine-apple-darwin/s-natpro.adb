with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Environment_Block;
with System.Native_IO;
with System.Shared_Locking;
with System.Standard_Allocators;
with System.Startup;
with System.Storage_Elements;
with System.Synchronous_Control;
with System.Zero_Terminated_Strings;
with C.errno;
with C.signal;
with C.spawn;
with C.stdlib;
with C.sys.wait;
with C.unistd;
package body System.Native_Processes is
   use Ada.Exception_Identification.From_Here;
   use type Ada.Command_Line.Exit_Status;
   use type Ada.Exception_Identification.Exception_Id;
   use type Storage_Elements.Storage_Offset;
   use type C.char;
   use type C.char_ptr;
   use type C.char_ptr_ptr; -- Command_Type
   use type C.ptrdiff_t;
   use type C.size_t;

   function strlen (s : not null access constant C.char) return C.size_t
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_strlen";

   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   package char_ptr_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char_ptr, C.char_ptr_ptr);

   function "+" (Left : C.char_ptr_ptr; Right : C.ptrdiff_t)
      return C.char_ptr_ptr
      with Convention => Intrinsic;
   pragma Inline_Always ("+");

   function "+" (Left : C.char_ptr_ptr; Right : C.ptrdiff_t)
      return C.char_ptr_ptr is
   begin
      return char_ptr_ptr_Conv.To_Pointer (
         char_ptr_ptr_Conv.To_Address (Left)
            + Storage_Elements.Storage_Offset (Right)
               * (C.char_ptr'Size / Standard'Storage_Unit));
   end "+";

   procedure Reallocate (X : in out Command_Type; Entry_Count : C.size_t);
   procedure Reallocate (X : in out Command_Type; Entry_Count : C.size_t) is
      Size : constant Storage_Elements.Storage_Count :=
         (Storage_Elements.Storage_Offset (Entry_Count) + 1) -- terminator
         * (C.char_ptr'Size / Standard'Storage_Unit);
   begin
      X := char_ptr_ptr_Conv.To_Pointer (
         Standard_Allocators.Reallocate (
            char_ptr_ptr_Conv.To_Address (X),
            Size));
   end Reallocate;

   function Entry_Count (Command : Command_Type) return C.size_t;
   function Entry_Count (Command : Command_Type) return C.size_t is
      Result : C.size_t := 0;
   begin
      if Command /= null then
         declare
            P : C.char_ptr_ptr := Command;
         begin
            while P.all /= null loop
               Result := Result + 1;
               P := P + 1;
            end loop;
         end;
      end if;
      return Result;
   end Entry_Count;

   function C_Width (Command : Command_Type) return C.size_t;
   function C_Width (Command : Command_Type) return C.size_t is
      Result : C.size_t := 0;
   begin
      if Command /= null then
         declare
            P : C.char_ptr_ptr := Command;
         begin
            while P.all /= null loop
               Result := Result
                  + strlen (P.all) * 2 -- for escape by '\'
                  + 1; -- Space
               P := P + 1;
            end loop;
         end;
      end if;
      return Result;
   end C_Width;

   procedure C_Image (
      Command : Command_Type;
      Command_Line : not null C.char_ptr);
   procedure C_Image (
      Command : Command_Type;
      Command_Line : not null C.char_ptr)
   is
      Length : C.size_t := 0;
   begin
      if Command /= null then
         declare
            P : C.char_ptr_ptr := Command;
         begin
            while P.all /= null loop
               declare
                  P_Length : constant C.size_t := strlen (P.all);
                  Command_Line_All : String (
                     1 .. Integer (Length + P_Length * 2 + 1));
                  for Command_Line_All'Address use
                     char_ptr_Conv.To_Address (Command_Line);
                  P_All : String (1 .. Natural (P_Length));
                  for P_All'Address use char_ptr_Conv.To_Address (P.all);
               begin
                  Append_Argument (Command_Line_All, Integer (Length), P_All);
               end;
               P := P + 1;
            end loop;
         end;
      end if;
      declare
         Command_Line_All : C.char_array (0 .. Length); -- NUL
         for Command_Line_All'Address use
            char_ptr_Conv.To_Address (Command_Line);
      begin
         Command_Line_All (Length) := C.char'Val (0);
      end;
   end C_Image;

   procedure Insert (
      Command : Command_Type;
      Index : C.ptrdiff_t;
      New_Item : String);
   procedure Insert (
      Command : Command_Type;
      Index : C.ptrdiff_t;
      New_Item : String)
   is
      P : constant C.char_ptr_ptr := Command + Index;
   begin
      --  clear for that any exception is raised
      P.all := null;
      --  set terminator
      C.char_ptr_ptr'(Command + (Index + 1)).all := null;
      --  allocate and copy
      P.all := char_ptr_Conv.To_Pointer (
         Standard_Allocators.Allocate (
            New_Item'Length * Zero_Terminated_Strings.Expanding + 1));
      Zero_Terminated_Strings.To_C (New_Item, P.all);
   end Insert;

   --  implementation

   procedure Free (X : in out Command_Type) is
   begin
      if X /= null then
         declare
            P : C.char_ptr_ptr := X;
         begin
            while P.all /= null loop
               Standard_Allocators.Free (char_ptr_Conv.To_Address (P.all));
               P := P + 1;
            end loop;
         end;
         Standard_Allocators.Free (char_ptr_ptr_Conv.To_Address (X));
         X := null;
      end if;
   end Free;

   function Image (Command : Command_Type) return String is
      Width : constant C.size_t := C_Width (Command);
      Command_Line : aliased C.char_array (0 .. Width); -- NUL
   begin
      C_Image (Command, Command_Line (0)'Unchecked_Access);
      return Zero_Terminated_Strings.Value (Command_Line (0)'Unchecked_Access);
   end Image;

   procedure Value (
      Command_Line : String;
      Command : aliased out Command_Type)
   is
      Capacity : C.size_t := 256;
      I : Positive := Command_Line'First;
      Argument_Index : C.ptrdiff_t := 0;
   begin
      Reallocate (Command, Capacity);
      Command.all := null; -- terminator
      loop
         --  skip spaces
         while I <= Command_Line'Last and then Command_Line (I) = ' ' loop
            I := I + 1;
         end loop;
         exit when I > Command_Line'Last;
         --  clear for that any exception is raised
         C.char_ptr_ptr'(Command + Argument_Index).all := null;
         --  set terminator
         C.char_ptr_ptr'(Command + (Argument_Index + 1)).all := null;
         --  unescape, allocate and copy
         declare
            Buffer : String (I .. Command_Line'Last);
            J : Natural := I - 1;
         begin
            loop
               if Command_Line (I) = '\' then
                  I := I + 1;
                  exit when I > Command_Line'Last;
               end if;
               J := J + 1;
               Buffer (J) := Command_Line (I);
               I := I + 1;
               exit when I > Command_Line'Last;
               if Command_Line (I) = ' ' then
                  I := I + 1;
                  exit;
               end if;
               exit when I > Command_Line'Last;
            end loop;
            Insert (Command, Argument_Index, Buffer (Buffer'First .. J));
         end;
         Argument_Index := Argument_Index + 1;
         if C.size_t (Argument_Index) >= Capacity then
            Capacity := Capacity * 2;
            Reallocate (Command, Capacity);
         end if;
      end loop;
   end Value;

   procedure Append (
      Command : aliased in out Command_Type;
      New_Item : String)
   is
      Old_Count : constant C.size_t := Entry_Count (Command);
   begin
      Reallocate (Command, Old_Count + 1);
      Insert (Command, C.ptrdiff_t (Old_Count), New_Item);
   end Append;

   procedure Append (
      Command : aliased in out Command_Type;
      First : Positive;
      Last : Natural)
   is
      pragma Assert (Last >= First);
      Old_Count : constant C.size_t := Entry_Count (Command);
      New_Count : constant C.size_t := Old_Count + C.size_t (Last - First + 1);
   begin
      Reallocate (Command, New_Count);
      --  clear for that any exception is raised, and set terminator
      for I in Old_Count .. New_Count loop
         C.char_ptr_ptr'(Command + C.ptrdiff_t (I)).all := null;
      end loop;
      --  allocate and copy
      for I in 0 .. Last - First loop
         declare
            P : constant C.char_ptr_ptr :=
               Command + C.ptrdiff_t (Old_Count) + C.ptrdiff_t (I);
            Q : constant C.char_ptr_ptr :=
               char_ptr_ptr_Conv.To_Pointer (Startup.argv)
               + C.ptrdiff_t (First)
               + C.ptrdiff_t (I);
            Length : constant C.size_t := strlen (Q.all);
         begin
            P.all := char_ptr_Conv.To_Pointer (
               Standard_Allocators.Allocate (
                  Storage_Elements.Storage_Offset (Length) + 1));
            declare
               P_All : C.char_array (0 .. Length);
               for P_All'Address use char_ptr_Conv.To_Address (P.all);
               Q_All : C.char_array (0 .. Length);
               for Q_All'Address use char_ptr_Conv.To_Address (Q.all);
            begin
               P_All := Q_All;
            end;
         end;
      end loop;
   end Append;

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
         declare
            E : Character renames Argument (I);
         begin
            if E = ' ' then
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
               Command_Line (Last) := E;
            end if;
         end;
      end loop;
   end Append_Argument;

   --  child process management

   procedure Spawn (
      Child : out C.sys.types.pid_t;
      Command : Command_Type;
      Directory : String;
      Search_Path : Boolean;
      Input : Native_IO.Handle_Type;
      Output : Native_IO.Handle_Type;
      Error : Native_IO.Handle_Type);
   procedure Spawn (
      Child : out C.sys.types.pid_t;
      Command : Command_Type;
      Directory : String;
      Search_Path : Boolean;
      Input : Native_IO.Handle_Type;
      Output : Native_IO.Handle_Type;
      Error : Native_IO.Handle_Type)
   is
      Directory_Length : constant C.size_t := Directory'Length;
      C_Directory : C.char_array (
         0 .. Directory_Length * Zero_Terminated_Strings.Expanding);
         --  This is a dynamic array and should have been allocated before
         --    below no exception area.
      Old_Directory : C.char_ptr := null;
      Actions : access C.spawn.posix_spawn_file_actions_t := null;
      Actions_Body : aliased C.spawn.posix_spawn_file_actions_t;
      Exception_Id : Ada.Exception_Identification.Exception_Id :=
         Ada.Exception_Identification.Null_Id;
   begin
      Shared_Locking.Enter; -- exclusive chdir, no exception from here
      --  set current directory
      if Directory_Length > 0 then
         Old_Directory := C.unistd.getcwd (null, 0);
         Zero_Terminated_Strings.To_C (Directory, C_Directory (0)'Access);
         if C.unistd.chdir (C_Directory (0)'Access) < 0 then
            Exception_Id := Name_Error'Identity;
         end if;
      end if;
      --  execute
      if Exception_Id = Ada.Exception_Identification.Null_Id then
         declare
            Environment_Block : constant C.char_ptr_ptr :=
               System.Environment_Block;
            subtype Handle_Index is C.signed_int range 0 .. 2;
            Source_Handles : array (Handle_Index) of C.signed_int;
            New_Child : aliased C.sys.types.pid_t;
            errno : C.signed_int;
         begin
            Source_Handles (0) := Input;
            Source_Handles (1) := Output;
            Source_Handles (2) := Error;
            for I in Handle_Index loop
               if Source_Handles (I) /= I then
                  if Actions = null then
                     if C.spawn.posix_spawn_file_actions_init (
                        Actions_Body'Access) /= 0
                     then
                        Exception_Id := Use_Error'Identity;
                        exit; -- failed
                     end if;
                     Actions := Actions_Body'Access;
                  end if;
                  if C.spawn.posix_spawn_file_actions_adddup2 (
                     Actions,
                     Source_Handles (I),
                     I) /= 0
                  then
                     Exception_Id := Use_Error'Identity;
                     exit; -- failed
                  end if;
               end if;
            end loop;
            if Exception_Id = Ada.Exception_Identification.Null_Id then
               if Search_Path then
                  errno := C.spawn.posix_spawnp (
                     New_Child'Access,
                     Command.all,
                     Actions,
                     null,
                     Command,
                     Environment_Block);
               else
                  errno := C.spawn.posix_spawn (
                     New_Child'Access,
                     Command.all,
                     Actions,
                     null,
                     Command,
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
                     Exception_Id := Native_IO.IO_Exception_Id (Error);
               end case;
            end if;
         end;
      end if;
      if Actions /= null then
         if C.spawn.posix_spawn_file_actions_destroy (Actions) /= 0 then
            if Exception_Id = Ada.Exception_Identification.Null_Id then
               Exception_Id := Use_Error'Identity;
            end if;
         end if;
      end if;
      --  restore current directory
      if Directory_Length > 0 then
         if C.unistd.chdir (Old_Directory) < 0 then
            if Exception_Id = Ada.Exception_Identification.Null_Id then
               Exception_Id := Name_Error'Identity;
            end if;
         end if;
         C.stdlib.free (C.void_ptr (char_ptr_Conv.To_Address (Old_Directory)));
      end if;
      Shared_Locking.Leave; -- no exception until here
      if Exception_Id /= Ada.Exception_Identification.Null_Id then
         Raise_Exception (Exception_Id);
      end if;
   end Spawn;

   function WIFEXITED (x : C.signed_int) return Boolean;
   function WIFEXITED (x : C.signed_int) return Boolean is
      WSTATUS : constant C.signed_int := x mod (8#177# + 1); -- x & 0177
   begin
      return WSTATUS = 0;
   end WIFEXITED;

   function WEXITSTATUS (x : C.signed_int) return C.signed_int;
   function WEXITSTATUS (x : C.signed_int) return C.signed_int is
   begin
      return x / 256;
   end WEXITSTATUS;

   function WIFSIGNALED (x : C.signed_int) return Boolean;
   function WIFSIGNALED (x : C.signed_int) return Boolean is
      WSTATUS : constant C.signed_int := x mod (8#177# + 1); -- x & 0177
   begin
      --  _WSTOPPED is 0177 in Darwin and FreeBSD.
      --  Both low 7 bits of __W_STOPCODE and __W_CONTINUED is 0x7f in Linux.
      return WSTATUS /= 8#177# and then WSTATUS /= 0;
   end WIFSIGNALED;

   procedure Wait (
      Child : in out Process;
      Options : C.signed_int;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status);
   procedure Wait (
      Child : in out Process;
      Options : C.signed_int;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status)
   is
      Code : aliased C.signed_int;
      Terminated_Child : C.sys.types.pid_t;
      errno : C.signed_int;
   begin
      Synchronous_Control.Unlock_Abort;
      Terminated_Child := C.sys.wait.waitpid (Child.Id, Code'Access, Options);
      errno := C.errno.errno;
      Synchronous_Control.Lock_Abort; -- raise if aborted
      if Terminated_Child < 0 then
         if errno /= C.errno.EINTR then
            Raise_Exception (Use_Error'Identity);
         end if;
         Terminated := False; -- interrupted and the signal is not "abort"
      elsif Terminated_Child = 0 then
         Terminated := False; -- WNOHANG
      else
         if WIFEXITED (Code) then
            Child.Id := -1;
            Status := Ada.Command_Line.Exit_Status (WEXITSTATUS (Code));
            Terminated := True; -- exited
         elsif WIFSIGNALED (Code) then
            Child.Id := -1;
            Status := -1;
            Terminated := True; -- signaled
         else
            Terminated := False; -- otherwise, WIFSTOPPED or WIFCONTINUED
         end if;
      end if;
   end Wait;

   procedure Kill (Child : Process; Signal : C.signed_int);
   procedure Kill (Child : Process; Signal : C.signed_int) is
   begin
      if C.signal.kill (Child.Id, Signal) < 0 then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Kill;

   --  implementation of child process management

   function Is_Open (Child : Process) return Boolean is
   begin
      return Child.Id /= -1;
   end Is_Open;

   procedure Create (
      Child : in out Process;
      Command : Command_Type;
      Directory : String := "";
      Search_Path : Boolean := False;
      Input : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Output : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Error : Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type) is
   begin
      Spawn (
         Child.Id,
         Command,
         Directory,
         Search_Path,
         Ada.Streams.Naked_Stream_IO.Handle (Input),
         Ada.Streams.Naked_Stream_IO.Handle (Output),
         Ada.Streams.Naked_Stream_IO.Handle (Error));
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
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (Command_Type, Free);
      Command : aliased Command_Type;
   begin
      Holder.Assign (Command);
      Value (Command_Line, Command);
      Spawn (
         Child.Id,
         Command,
         Directory,
         Search_Path,
         Ada.Streams.Naked_Stream_IO.Handle (Input),
         Ada.Streams.Naked_Stream_IO.Handle (Output),
         Ada.Streams.Naked_Stream_IO.Handle (Error));
   end Create;

   procedure Wait (
      Child : in out Process;
      Status : out Ada.Command_Line.Exit_Status) is
   begin
      loop
         declare
            Terminated : Boolean;
         begin
            Wait (Child, 0, Terminated => Terminated, Status => Status);
            exit when Terminated;
         end;
      end loop;
   end Wait;

   procedure Wait_Immediate (
      Child : in out Process;
      Terminated : out Boolean;
      Status : out Ada.Command_Line.Exit_Status) is
   begin
      Wait (Child, C.sys.wait.WNOHANG,
         Terminated => Terminated, Status => Status);
   end Wait_Immediate;

   procedure Abort_Process (Child : in out Process) is
   begin
      Kill (Child, C.signal.SIGTERM);
   end Abort_Process;

   procedure Forced_Abort_Process (Child : in out Process) is
   begin
      Kill (Child, C.signal.SIGKILL);
   end Forced_Abort_Process;

   --  pass a command to the shell

   procedure C_Shell (
      Command_Line : C.char_ptr;
      Status : out Ada.Command_Line.Exit_Status);
   procedure C_Shell (
      Command_Line : C.char_ptr;
      Status : out Ada.Command_Line.Exit_Status)
   is
      Code : C.signed_int;
   begin
      Code := C.stdlib.C_system (Command_Line);
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
   end C_Shell;

   --  implementation of pass a command to the shell

   procedure Shell (
      Command : Command_Type;
      Status : out Ada.Command_Line.Exit_Status)
   is
      Width : constant C.size_t := C_Width (Command);
      Command_Line : aliased C.char_array (0 .. Width); -- NUL
   begin
      C_Image (Command, Command_Line (0)'Unchecked_Access);
      C_Shell (Command_Line (0)'Unchecked_Access, Status);
   end Shell;

   procedure Shell (
      Command_Line : String;
      Status : out Ada.Command_Line.Exit_Status)
   is
      C_Command_Line : C.char_array (
         0 ..
         Command_Line'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Command_Line, C_Command_Line (0)'Access);
      C_Shell (C_Command_Line (0)'Unchecked_Access, Status);
   end Shell;

end System.Native_Processes;
