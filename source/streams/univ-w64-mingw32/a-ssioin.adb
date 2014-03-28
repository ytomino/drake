with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Form_Parameters;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.string;
with C.winbase;
with C.wincon;
with C.windef;
with C.winerror;
package body Ada.Streams.Stream_IO.Inside is
   use Exception_Identification.From_Here;
   use type IO_Modes.File_Shared;
   use type IO_Modes.File_Shared_Spec;
   use type Tags.Tag;
   use type System.Storage_Elements.Storage_Offset;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.UINT;
   use type C.windef.WINBOOL;
   use type C.winnt.LONGLONG;
--  use type System.Address;
--  use type C.winnt.HANDLE; -- C.void_ptr

   --  the parameter Form

   procedure Set (Form : in out Packed_Form; Keyword, Item : String) is
   begin
      if Keyword = "shared" then
         if Item'Length > 0
            and then (
               Item (Item'First) = 'a' -- allow
               or else Item (Item'First) = 'y') -- yes, compatibility
         then
            Form.Shared := IO_Modes.Allow;
         elsif Item'Length > 0 and then Item (Item'First) = 'r' then -- read
            Form.Shared := IO_Modes.Read_Only;
         elsif Item'Length > 0 and then Item (Item'First) = 'd' then -- deny
            Form.Shared := IO_Modes.Deny;
         elsif Item'Length > 0 and then Item (Item'First) = 'n' then -- no
            Form.Shared := IO_Modes.By_Mode;
         end if;
      elsif Keyword = "wait" then
         if Item'Length > 0 and then Item (Item'First) = 'f' then -- false
            Form.Wait := False;
         elsif Item'Length > 0 and then Item (Item'First) = 't' then -- true
            Form.Wait := True;
         end if;
      elsif Keyword = "overwrite" then
         if Item'Length > 0 and then Item (Item'First) = 'f' then -- false
            Form.Overwrite := False;
         elsif Item'Length > 0 and then Item (Item'First) = 't' then -- true
            Form.Overwrite := True;
         end if;
      end if;
   end Set;

   function Pack (Form : String) return Packed_Form is
      Keyword_First : Positive;
      Keyword_Last : Natural;
      Item_First : Positive;
      Item_Last : Natural;
      Last : Natural;
   begin
      return Result : Packed_Form := Default_Form do
         Last := Form'First - 1;
         while Last < Form'Last loop
            System.Form_Parameters.Get (
               Form (Last + 1 .. Form'Last),
               Keyword_First,
               Keyword_Last,
               Item_First,
               Item_Last,
               Last);
            Set (
               Result,
               Form (Keyword_First .. Keyword_Last),
               Form (Item_First .. Item_Last));
         end loop;
      end return;
   end Pack;

   procedure Unpack (
      Form : Packed_Form;
      Result : out Form_String;
      Last : out Natural)
   is
      New_Last : Natural;
   begin
      Last := Form_String'First - 1;
      if Form.Shared /= IO_Modes.By_Mode then
         case IO_Modes.File_Shared (Form.Shared) is
            when IO_Modes.Allow =>
               New_Last := Last + 10;
               Result (Last + 1 .. New_Last) := "shared=yes";
               Last := New_Last;
            when IO_Modes.Read_Only =>
               New_Last := Last + 11;
               Result (Last + 1 .. New_Last) := "shared=read";
               Last := New_Last;
            when IO_Modes.Deny =>
               New_Last := Last + 12;
               Result (Last + 1 .. New_Last) := "shared=write";
               Last := New_Last;
         end case;
      end if;
      if Form.Wait then
         if Last /= Form_String'First - 1 then
            New_Last := Last + 1;
            Result (New_Last) := ',';
            Last := New_Last;
         end if;
         New_Last := Last + 9;
         Result (Last + 1 .. New_Last) := "wait=true";
         Last := New_Last;
      end if;
      if Form.Overwrite then
         if Last /= Form_String'First - 1 then
            New_Last := Last + 1;
            Result (New_Last) := ',';
            Last := New_Last;
         end if;
         New_Last := Last + 14;
         Result (Last + 1 .. New_Last) := "overwrite=true";
         Last := New_Last;
      end if;
   end Unpack;

   --  handle

   procedure CloseHandle (
      Handle : Handle_Type;
      Raise_On_Error : Boolean);
   procedure CloseHandle (
      Handle : Handle_Type;
      Raise_On_Error : Boolean)
   is
      Error : Boolean;
   begin
      Error := C.winbase.CloseHandle (Handle) = 0;
      --  CloseHandle remove the temporary file
      if Error and then Raise_On_Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end CloseHandle;

   procedure SetFilePointerEx (
      Handle : C.winnt.HANDLE;
      DistanceToMove : C.winnt.LONGLONG;
      NewFilePointer : out C.winnt.LONGLONG;
      MoveMethod : C.windef.DWORD);
   procedure SetFilePointerEx (
      Handle : C.winnt.HANDLE;
      DistanceToMove : C.winnt.LONGLONG;
      NewFilePointer : out C.winnt.LONGLONG;
      MoveMethod : C.windef.DWORD)
   is
      liDistanceToMove : C.winnt.LARGE_INTEGER;
      liNewFilePointer : aliased C.winnt.LARGE_INTEGER;
   begin
      liDistanceToMove.QuadPart := DistanceToMove;
      if C.winbase.SetFilePointerEx (
         Handle,
         liDistanceToMove,
         liNewFilePointer'Access,
         MoveMethod) = 0
      then
         Raise_Exception (Use_Error'Identity);
      end if;
      NewFilePointer := liNewFilePointer.QuadPart;
   end SetFilePointerEx;

   procedure GetFileSizeEx (
      Handle : C.winnt.HANDLE;
      FileSize : out C.winnt.LONGLONG);
   procedure GetFileSizeEx (
      Handle : C.winnt.HANDLE;
      FileSize : out C.winnt.LONGLONG)
   is
      liFileSize : aliased C.winnt.LARGE_INTEGER;
   begin
      if C.winbase.GetFileSizeEx (Handle, liFileSize'Access) = 0 then
         Raise_Exception (Use_Error'Identity);
      end if;
      FileSize := liFileSize.QuadPart;
   end GetFileSizeEx;

   --  implementation of handle

   function Is_Terminal (Handle : Handle_Type) return Boolean is
      Mode : aliased C.windef.DWORD;
   begin
      return C.winbase.GetFileType (Handle) = C.winbase.FILE_TYPE_CHAR
         and then C.wincon.GetConsoleMode (Handle, Mode'Access) /= 0;
   end Is_Terminal;

   function Is_Seekable (Handle : Handle_Type) return Boolean is
   begin
      return C.winbase.SetFilePointerEx (
         Handle,
         (Unchecked_Tag => 2, QuadPart => 0),
         null,
         C.winbase.FILE_CURRENT) /= 0;
   end Is_Seekable;

   --  implementation of handle for controlled

   procedure Open (
      File : in out File_Type;
      Handle : Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : Packed_Form := Default_Form;
      To_Close : Boolean := False) is
   begin
      Open (
         Reference (File).all,
         Handle,
         Mode,
         Name => Name,
         Form => Form,
         To_Close => To_Close);
   end Open;

   function Handle (File : File_Type) return Handle_Type is
   begin
      return Handle (Reference (File).all);
   end Handle;

   function Non_Controlled (File : File_Type)
      return not null access Non_Controlled_File_Type is
   begin
      return Reference (File);
   end Non_Controlled;

   --  non-controlled

   package LPWSTR_Conv is
      new System.Address_To_Named_Access_Conversions (
         C.winnt.WCHAR,
         C.winnt.LPWSTR);

   procedure Finally (X : not null access C.winnt.LPWSTR);
   procedure Finally (X : not null access C.winnt.LPWSTR) is
   begin
      System.Standard_Allocators.Free (LPWSTR_Conv.To_Address (X.all));
   end Finally;

   function Allocate (
      Handle : Handle_Type;
      Mode : File_Mode;
      Kind : Stream_Kind;
      Name : C.winnt.LPWSTR;
      Name_Length : C.size_t;
      Form : Packed_Form)
      return Non_Controlled_File_Type;
   function Allocate (
      Handle : Handle_Type;
      Mode : File_Mode;
      Kind : Stream_Kind;
      Name : C.winnt.LPWSTR;
      Name_Length : C.size_t;
      Form : Packed_Form)
      return Non_Controlled_File_Type is
   begin
      return new Stream_Type'(
         Handle => Handle,
         Mode => Mode,
         Kind => Kind,
         Buffer_Inline => <>,
         Name => Name,
         Name_Length => Name_Length,
         Form => Form,
         Buffer => System.Null_Address,
         Buffer_Length => Uninitialized_Buffer,
         Buffer_Index => 0,
         Reading_Index => 0,
         Writing_Index => 0,
         Dispatcher => (
            Tag => Tags.No_Tag,
            File => null));
   end Allocate;

   procedure Free (File : in out Non_Controlled_File_Type);
   procedure Free (File : in out Non_Controlled_File_Type) is
      use type System.Address;
      procedure Raw_Free is
         new Unchecked_Deallocation (Stream_Type, Non_Controlled_File_Type);
   begin
      if File.Buffer /= File.Buffer_Inline'Address then
         System.Standard_Allocators.Free (File.Buffer);
      end if;
      System.Standard_Allocators.Free (LPWSTR_Conv.To_Address (File.Name));
      Raw_Free (File);
   end Free;

   type Scoped_Handle_And_File is record
      Handle : aliased Handle_Type;
      File : aliased Non_Controlled_File_Type;
   end record;
   pragma Suppress_Initialization (Scoped_Handle_And_File);

   procedure Finally (X : not null access Scoped_Handle_And_File);
   procedure Finally (X : not null access Scoped_Handle_And_File) is
      use type C.winnt.HANDLE;
   begin
      if X.Handle /= C.winbase.INVALID_HANDLE_VALUE then
         CloseHandle (X.Handle, Raise_On_Error => False);
      end if;
      if X.File /= null then
         Free (X.File);
      end if;
   end Finally;

   type Scoped_Handle_And_File_And_Name is record
      Super : aliased Scoped_Handle_And_File;
      Name : aliased C.winnt.LPWSTR;
   end record;
   pragma Suppress_Initialization (Scoped_Handle_And_File_And_Name);

   procedure Finally (X : not null access Scoped_Handle_And_File_And_Name);
   procedure Finally (X : not null access Scoped_Handle_And_File_And_Name) is
   begin
      Finally (X.Super'Access);
      Finally (X.Name'Access);
   end Finally;

   procedure Set_Buffer_Index (
      File : not null Non_Controlled_File_Type;
      Buffer_Index : Stream_Element_Offset);
   procedure Set_Buffer_Index (
      File : not null Non_Controlled_File_Type;
      Buffer_Index : Stream_Element_Offset) is
   begin
      if File.Buffer_Length = Uninitialized_Buffer then
         File.Buffer_Index := Buffer_Index;
      elsif File.Buffer_Length = 0 then
         File.Buffer_Index := 0;
      else
         File.Buffer_Index := Buffer_Index rem File.Buffer_Length;
      end if;
      File.Reading_Index := File.Buffer_Index;
      File.Writing_Index := File.Buffer_Index;
   end Set_Buffer_Index;

   procedure Set_Index_To_Append (File : not null Non_Controlled_File_Type);
   procedure Set_Index_To_Append (File : not null Non_Controlled_File_Type) is
      Z_Index : C.winnt.LONGLONG;
   begin
      SetFilePointerEx (File.Handle, 0, Z_Index, C.winbase.FILE_END);
      Set_Buffer_Index (File, Stream_Element_Offset (Z_Index));
   end Set_Index_To_Append;

   Temp_Prefix : constant C.winnt.WCHAR_array (0 .. 3) := (
      C.winnt.WCHAR'Val (Wide_Character'Pos ('A')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('D')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('A')),
      C.winnt.WCHAR'Val (0));

   procedure Open_Temporary_File (
      Handle : aliased out Handle_Type; -- held
      Full_Name : aliased out C.winnt.LPWSTR; -- held
      Full_Name_Length : out C.size_t);
   procedure Open_Temporary_File (
      Handle : aliased out Handle_Type;
      Full_Name : aliased out C.winnt.LPWSTR;
      Full_Name_Length : out C.size_t)
   is
      use type C.winnt.HANDLE;
      Temp_Dir : C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
      Temp_Name : C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
   begin
      --  compose template
      if C.winbase.GetTempPath (Temp_Dir'Length, Temp_Dir (0)'Access) = 0
         or else C.winbase.GetTempFileName (
            Temp_Dir (0)'Access,
            Temp_Prefix (0)'Access,
            0,
            Temp_Name (0)'Access) = 0
      then
         Raise_Exception (Use_Error'Identity);
      end if;
      --  open
      Handle := C.winbase.CreateFile (
         lpFileName => Temp_Name (0)'Access,
         dwDesiredAccess =>
            C.winnt.GENERIC_READ
            or C.winnt.GENERIC_WRITE, -- full access for Reset/Set_Mode
         dwShareMode =>
            C.winnt.FILE_SHARE_READ
            or C.winnt.FILE_SHARE_WRITE,
         lpSecurityAttributes => null,
         dwCreationDisposition =>
            C.winbase.OPEN_EXISTING
            or C.winbase.TRUNCATE_EXISTING,
         dwFlagsAndAttributes =>
            C.winnt.FILE_ATTRIBUTE_TEMPORARY
            or C.winbase.FILE_FLAG_DELETE_ON_CLOSE,
         hTemplateFile => C.winnt.HANDLE (System.Null_Address));
      if Handle = C.winbase.INVALID_HANDLE_VALUE then
         Raise_Exception (Use_Error'Identity);
      end if;
      --  allocate filename
      Full_Name_Length := C.string.wcslen (Temp_Name (0)'Access);
      Full_Name := LPWSTR_Conv.To_Pointer (
         System.Standard_Allocators.Allocate (
            System.Storage_Elements.Storage_Offset (Full_Name_Length + 1)
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      declare
         Full_Name_A : C.winnt.WCHAR_array (C.size_t);
         for Full_Name_A'Address use LPWSTR_Conv.To_Address (Full_Name);
      begin
         Full_Name_A (0 .. Full_Name_Length) :=
            Temp_Name (0 .. Full_Name_Length);
      end;
   end Open_Temporary_File;

   procedure Compose_File_Name (
      Name : String;
      Full_Name : aliased out C.winnt.LPWSTR; -- held
      Full_Name_Length : out C.size_t);
   procedure Compose_File_Name (
      Name : String;
      Full_Name : aliased out C.winnt.LPWSTR;
      Full_Name_Length : out C.size_t)
   is
      W_Name : C.winnt.WCHAR_array (
         0 ..
         Name'Length * System.Zero_Terminated_WStrings.Expanding);
      W_Name_Length : C.size_t;
      Full_Path : C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
      Full_Path_Length : C.size_t;
   begin
      System.Zero_Terminated_WStrings.To_C (
         Name,
         W_Name (0)'Access,
         W_Name_Length);
      Full_Path_Length := C.size_t (C.winbase.GetFullPathName (
         W_Name (0)'Access,
         Full_Path'Length,
         Full_Path (0)'Access,
         null));
      if Full_Path_Length = 0 then
         Full_Path_Length := W_Name_Length;
         Full_Path (0 .. Full_Path_Length) := W_Name (0 .. W_Name_Length);
      end if;
      --  allocate filename
      Full_Name_Length := Full_Path_Length;
      Full_Name := LPWSTR_Conv.To_Pointer (
         System.Standard_Allocators.Allocate (
            System.Storage_Elements.Storage_Offset (Full_Name_Length + 1)
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      declare
         Full_Name_A : C.winnt.WCHAR_array (C.size_t);
         for Full_Name_A'Address use LPWSTR_Conv.To_Address (Full_Name);
      begin
         Full_Name_A (0 .. Full_Name_Length) :=
            Full_Path (0 .. Full_Name_Length);
      end;
   end Compose_File_Name;

   type Open_Method is (Open, Create, Reset);
   pragma Discard_Names (Open_Method);

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type; -- held
      Mode : File_Mode;
      Name : not null C.winnt.LPWSTR;
      Form : Packed_Form);
   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : File_Mode;
      Name : not null C.winnt.LPWSTR;
      Form : Packed_Form)
   is
      use type C.winnt.HANDLE;
      DesiredAccess : C.windef.DWORD;
      ShareMode : C.windef.DWORD;
      CreationDisposition : C.windef.DWORD;
      Shared : IO_Modes.File_Shared;
      Error : C.windef.DWORD;
   begin
      --  Flags, Append_File always has read and write access for Inout_File
      if Form.Shared /= IO_Modes.By_Mode then
         Shared := IO_Modes.File_Shared (Form.Shared);
      else
         case Mode is
            when In_File =>
               Shared := IO_Modes.Read_Only;
            when Out_File | Append_File =>
               Shared := IO_Modes.Deny;
         end case;
      end if;
      case Method is
         when Create =>
            declare
               use C.winbase, C.winnt;
               Access_Modes : constant array (File_Mode) of C.windef.DWORD := (
                  In_File => GENERIC_READ or GENERIC_WRITE,
                  Out_File => GENERIC_WRITE,
                  Append_File => GENERIC_READ or GENERIC_WRITE);
               Creations : constant array (File_Mode) of C.windef.DWORD := (
                  In_File => CREATE_ALWAYS,
                  Out_File => CREATE_ALWAYS,
                  Append_File => OPEN_ALWAYS); -- no truncation
            begin
               DesiredAccess := Access_Modes (Mode);
               CreationDisposition := Creations (Mode);
               Shared := IO_Modes.Deny;
               if not Form.Overwrite then
                  CreationDisposition := CREATE_NEW;
               end if;
            end;
         when Open =>
            declare
               use C.winbase, C.winnt;
               Access_Modes : constant array (File_Mode) of C.windef.DWORD := (
                  In_File => GENERIC_READ,
                  Out_File => GENERIC_WRITE,
                  Append_File => GENERIC_READ or GENERIC_WRITE);
               Creations : constant array (File_Mode) of C.windef.DWORD := (
                  In_File => OPEN_EXISTING,
                  Out_File => TRUNCATE_EXISTING,
                  Append_File => OPEN_EXISTING);
            begin
               DesiredAccess := Access_Modes (Mode);
               CreationDisposition := Creations (Mode);
            end;
         when Reset =>
            declare
               use C.winbase, C.winnt;
               Access_Modes : constant array (File_Mode) of C.windef.DWORD := (
                  In_File => GENERIC_READ,
                  Out_File => GENERIC_WRITE,
                  Append_File => GENERIC_READ or GENERIC_WRITE);
            begin
               DesiredAccess := Access_Modes (Mode);
               CreationDisposition := OPEN_EXISTING; -- no truncation
            end;
      end case;
      if Shared /= IO_Modes.Allow then
         if Form.Wait then
            --  use LockFileEx
            ShareMode := C.winnt.FILE_SHARE_READ or C.winnt.FILE_SHARE_WRITE;
         else
            declare
               Lock_Flags : constant array (IO_Modes.File_Shared range
                  IO_Modes.Read_Only .. IO_Modes.Deny) of C.windef.DWORD := (
                     IO_Modes.Read_Only => C.winnt.FILE_SHARE_READ,
                     IO_Modes.Deny => 0);
            begin
               ShareMode := Lock_Flags (Shared);
            end;
         end if;
      else
         ShareMode := C.winnt.FILE_SHARE_READ or C.winnt.FILE_SHARE_WRITE;
      end if;
      --  open
      Handle := C.winbase.CreateFile (
         lpFileName => Name,
         dwDesiredAccess => DesiredAccess,
         dwShareMode => ShareMode,
         lpSecurityAttributes => null,
         dwCreationDisposition => CreationDisposition,
         dwFlagsAndAttributes => C.winnt.FILE_ATTRIBUTE_NORMAL,
         hTemplateFile => C.winnt.HANDLE (System.Null_Address));
      if Handle = C.winbase.INVALID_HANDLE_VALUE then
         Error := C.winbase.GetLastError;
         case Error is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_PATH_NOT_FOUND
               | C.winerror.ERROR_FILE_EXISTS -- CREATE_NEW
               | C.winerror.ERROR_INVALID_NAME
               | C.winerror.ERROR_ALREADY_EXISTS =>
               Raise_Exception (Name_Error'Identity);
            when C.winerror.ERROR_SHARING_VIOLATION =>
               Raise_Exception (Tasking_Error'Identity);
               --  Is Tasking_Error suitable?
            when others =>
               Raise_Exception (Use_Error'Identity);
         end case;
      end if;
      if Shared /= IO_Modes.Allow and then Form.Wait then
         declare
            Flags : constant array (IO_Modes.File_Shared range
               IO_Modes.Read_Only .. IO_Modes.Deny) of C.windef.DWORD := (
                  IO_Modes.Read_Only => 0,
                  IO_Modes.Deny => C.winbase.LOCKFILE_EXCLUSIVE_LOCK);
            Overlapped : aliased C.winbase.OVERLAPPED := (
               0, 0, (0, 0, 0), C.winnt.HANDLE (System.Null_Address));
            Dummy : C.windef.WINBOOL;
            pragma Unreferenced (Dummy);
         begin
            if C.winbase.LockFileEx (
               hFile => Handle,
               dwFlags => Flags (Shared),
               dwReserved => 0,
               nNumberOfBytesToLockLow => C.windef.DWORD'Last,
               nNumberOfBytesToLockHigh => C.windef.DWORD'Last,
               lpOverlapped => Overlapped'Access) = 0
            then
               Raise_Exception (Tasking_Error'Identity);
               --  Is Tasking_Error suitable?
            end if;
         end;
      end if;
   end Open_Ordinary;

   procedure Allocate_And_Open (
      Method : Open_Method;
      File : out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : Packed_Form);
   procedure Allocate_And_Open (
      Method : Open_Method;
      File : out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : Packed_Form)
   is
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            Scoped_Handle_And_File_And_Name,
            Finally);
      Scoped : aliased Scoped_Handle_And_File_And_Name :=
         ((C.winbase.INVALID_HANDLE_VALUE, null), null);
      Kind : Stream_Kind;
   begin
      Holder.Assign (Scoped'Access);
      if Name /= "" then
         Kind := Ordinary;
      else
         Kind := Temporary;
      end if;
      declare
         Full_Name_Length : C.size_t;
      begin
         if Kind = Ordinary then
            Compose_File_Name (Name, Scoped.Name, Full_Name_Length);
            Open_Ordinary (
               Method => Method,
               Handle => Scoped.Super.Handle,
               Mode => Mode,
               Name => Scoped.Name,
               Form => Form);
         else
            Open_Temporary_File (
               Handle => Scoped.Super.Handle,
               Full_Name => Scoped.Name,
               Full_Name_Length => Full_Name_Length);
         end if;
         Scoped.Super.File := Allocate (
            Handle => Scoped.Super.Handle,
            Mode => Mode,
            Kind => Kind,
            Name => Scoped.Name,
            Name_Length => Full_Name_Length,
            Form => Form);
         --  Scoped.Super.File holds Scoped.Name
         Scoped.Name := null;
      end;
      if Kind = Ordinary and then Mode = Append_File then
         Set_Index_To_Append (Scoped.Super.File); -- sets index to the last
      end if;
      File := Scoped.Super.File;
      --  complete
      Holder.Clear;
   end Allocate_And_Open;

   procedure Check_File_Open (File : Non_Controlled_File_Type);
   procedure Check_File_Open (File : Non_Controlled_File_Type) is
   begin
      if File = null then
         Raise_Exception (Status_Error'Identity);
      end if;
   end Check_File_Open;

   procedure Get_Buffer (File : not null Non_Controlled_File_Type);
   procedure Get_Buffer (File : not null Non_Controlled_File_Type) is
   begin
      if File.Buffer_Length = Uninitialized_Buffer then
         declare
            File_Type : C.windef.DWORD;
         begin
            File_Type := C.winbase.GetFileType (File.Handle);
            if File_Type /= C.winbase.FILE_TYPE_DISK then
               --  no buffering for terminal, pipe and unknown device
               File.Buffer_Length := 0;
            else
               --  disk file
               File.Buffer_Length :=
                  Stream_Element_Offset (System.Standard_Allocators.Page_Size);
            end if;
         end;
         if File.Buffer_Length = 0 then
            File.Buffer := File.Buffer_Inline'Address;
            File.Buffer_Index := 0;
         else
            File.Buffer := System.Standard_Allocators.Allocate (
               System.Storage_Elements.Storage_Count (File.Buffer_Length));
            File.Buffer_Index := File.Buffer_Index rem File.Buffer_Length;
         end if;
         File.Reading_Index := File.Buffer_Index;
         File.Writing_Index := File.Buffer_Index;
      end if;
   end Get_Buffer;

   procedure Ready_Reading_Buffer (
      File : not null Non_Controlled_File_Type;
      Error : out Boolean);
   procedure Ready_Reading_Buffer (
      File : not null Non_Controlled_File_Type;
      Error : out Boolean)
   is
      Buffer_Length : constant Stream_Element_Count :=
         Stream_Element_Offset'Max (1, File.Buffer_Length);
   begin
      --  reading buffer is from File.Reading_Index until File.Buffer_Index
      File.Buffer_Index := File.Buffer_Index rem Buffer_Length;
      File.Reading_Index := File.Buffer_Index;
      declare
         Read_Size : aliased C.windef.DWORD;
      begin
         Error := C.winbase.ReadFile (
            File.Handle,
            C.windef.LPVOID (File.Buffer
               + System.Storage_Elements.Storage_Offset (
                  File.Buffer_Index)),
            C.windef.DWORD (Buffer_Length - File.Buffer_Index),
            Read_Size'Access,
            lpOverlapped => null) = 0;
         if Error then
            case C.winbase.GetLastError is
               when C.winerror.ERROR_BROKEN_PIPE
                  | C.winerror.ERROR_NO_DATA =>
                  --  closed pipe
                  --  this subprogram is called from End_Of_File
                  --  because no buffering on pipe
                  Error := False;
               when others =>
                  null;
            end case;
         else
            File.Buffer_Index :=
               File.Buffer_Index + Stream_Element_Offset (Read_Size);
         end if;
      end;
      File.Writing_Index := File.Buffer_Index;
   end Ready_Reading_Buffer;

   procedure Reset_Reading_Buffer (File : not null Non_Controlled_File_Type);
   procedure Reset_Reading_Buffer (File : not null Non_Controlled_File_Type) is
      Dummy : C.winnt.LONGLONG;
      pragma Unreferenced (Dummy);
   begin
      SetFilePointerEx (
         File.Handle,
         C.winnt.LONGLONG (File.Reading_Index - File.Buffer_Index),
         Dummy,
         C.winbase.FILE_CURRENT);
      File.Buffer_Index := File.Reading_Index;
      File.Writing_Index := File.Buffer_Index;
   end Reset_Reading_Buffer;

   procedure Ready_Writing_Buffer (File : not null Non_Controlled_File_Type);
   procedure Ready_Writing_Buffer (File : not null Non_Controlled_File_Type) is
   begin
      --  writing buffer is from File.Buffer_Index until File.Writing_Index
      File.Buffer_Index := File.Buffer_Index rem File.Buffer_Length;
      File.Writing_Index := File.Buffer_Index;
      File.Reading_Index := File.Buffer_Index;
   end Ready_Writing_Buffer;

   procedure Flush_Writing_Buffer (
      File : not null Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True);
   procedure Flush_Writing_Buffer (
      File : not null Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True) is
   begin
      if File.Writing_Index > File.Buffer_Index then
         declare
            Error : Boolean := False;
            Written : aliased C.windef.DWORD;
         begin
            if C.winbase.WriteFile (
               File.Handle,
               C.windef.LPCVOID (File.Buffer
                  + System.Storage_Elements.Storage_Offset (
                     File.Buffer_Index)),
               C.windef.DWORD (File.Writing_Index - File.Buffer_Index),
               Written'Access,
               lpOverlapped => null) = 0
            then
               case C.winbase.GetLastError is
                  when C.winerror.ERROR_BROKEN_PIPE
                     | C.winerror.ERROR_NO_DATA =>
                     null;
                  when others =>
                     if Raise_On_Error then
                        Raise_Exception (Device_Error'Identity);
                     end if;
                     Error := True;
               end case;
            end if;
            if not Error then
               File.Buffer_Index := File.Writing_Index rem File.Buffer_Length;
               File.Writing_Index := File.Buffer_Index;
               File.Reading_Index := File.Buffer_Index;
            end if;
         end;
      end if;
   end Flush_Writing_Buffer;

   function Offset_Of_Buffer (File : not null Non_Controlled_File_Type)
      return Stream_Element_Offset;
   function Offset_Of_Buffer (File : not null Non_Controlled_File_Type)
      return Stream_Element_Offset is
   begin
      return (File.Writing_Index - File.Buffer_Index)
         - (File.Buffer_Index - File.Reading_Index);
   end Offset_Of_Buffer;

   procedure Read_From_Buffer (
      File : not null Non_Controlled_File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);
   procedure Read_From_Buffer (
      File : not null Non_Controlled_File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Taking_Length : constant Stream_Element_Offset :=
         Stream_Element_Offset'Min (
            Item'Last - Item'First + 1,
            File.Buffer_Index - File.Reading_Index);
      Buffer : Stream_Element_Array (Stream_Element_Count);
      for Buffer'Address use File.Buffer;
   begin
      Last := Item'First + Taking_Length - 1;
      Item (Item'First .. Last) := Buffer (
         File.Reading_Index ..
         File.Reading_Index + Taking_Length - 1);
      File.Reading_Index := File.Reading_Index + Taking_Length;
   end Read_From_Buffer;

   procedure Write_To_Buffer (
      File : not null Non_Controlled_File_Type;
      Item : Stream_Element_Array;
      First : out Stream_Element_Offset);
   procedure Write_To_Buffer (
      File : not null Non_Controlled_File_Type;
      Item : Stream_Element_Array;
      First : out Stream_Element_Offset)
   is
      Taking_Length : constant Stream_Element_Offset :=
         Stream_Element_Offset'Min (
            Item'Last - Item'First + 1,
            File.Buffer_Length - File.Writing_Index);
      Buffer : Stream_Element_Array (Stream_Element_Count);
      for Buffer'Address use File.Buffer;
      Last : Stream_Element_Count;
   begin
      First := Item'First + Taking_Length;
      Last := First - 1;
      Buffer (
         File.Writing_Index ..
         File.Writing_Index + Taking_Length - 1) := Item (Item'First .. Last);
      File.Writing_Index := File.Writing_Index + Taking_Length;
   end Write_To_Buffer;

   --  implementation of non-controlled

   procedure Create (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : Packed_Form := Default_Form) is
   begin
      if File /= null then
         Raise_Exception (Status_Error'Identity);
      end if;
      Allocate_And_Open (
         Method => Create,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
   end Create;

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : Packed_Form := Default_Form) is
   begin
      if File /= null then
         Raise_Exception (Status_Error'Identity);
      end if;
      Allocate_And_Open (
         Method => Open,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
   end Open;

   procedure Close (
      File : aliased in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True) is
   begin
      Check_File_Open (File);
      declare
         use type C.winnt.HANDLE;
         package Holder is
            new Exceptions.Finally.Scoped_Holder (
               Scoped_Handle_And_File,
               Finally);
         Scoped : aliased Scoped_Handle_And_File :=
            (C.winbase.INVALID_HANDLE_VALUE, null);
         Freeing_File : constant Non_Controlled_File_Type := File;
         Kind : constant Stream_Kind := File.all.Kind;
      begin
         Holder.Assign (Scoped'Access);
         case Kind is
            when Ordinary | Temporary | External | External_No_Close =>
               Scoped.File := Freeing_File;
            when Standard_Handle =>
               null; -- statically allocated
         end case;
         File := null;
         case Freeing_File.Kind is
            when Ordinary | Temporary | External =>
               Scoped.Handle := Freeing_File.Handle;
            when External_No_Close | Standard_Handle =>
               null;
         end case;
         if Freeing_File.Kind /= Temporary then
            Flush_Writing_Buffer (
               Freeing_File,
               Raise_On_Error => Raise_On_Error);
         end if;
         if Scoped.Handle /= C.winbase.INVALID_HANDLE_VALUE then
            --  close explicitly in below
            Scoped.Handle := C.winbase.INVALID_HANDLE_VALUE;
            CloseHandle (Freeing_File.Handle, Raise_On_Error => True);
         end if;
      end;
   end Close;

   procedure Delete (File : aliased in out Non_Controlled_File_Type) is
   begin
      Check_File_Open (File);
      case File.all.Kind is
         when Ordinary =>
            declare
               Deleting_File_Name : C.winnt.WCHAR_array (
                  0 ..
                  File.all.Name_Length);
            begin
               declare
                  File_Name_A : C.winnt.WCHAR_array (C.size_t);
                  for File_Name_A'Address use
                     LPWSTR_Conv.To_Address (File.all.Name);
               begin
                  Deleting_File_Name := File_Name_A (
                     0 ..
                     File.all.Name_Length);
               end;
               Close (File, Raise_On_Error => True);
               if C.winbase.DeleteFile (Deleting_File_Name (0)'Access) = 0 then
                  Raise_Exception (Use_Error'Identity);
               end if;
            end;
         when Temporary =>
            Close (File, Raise_On_Error => True);
         when External | External_No_Close | Standard_Handle =>
            Raise_Exception (Status_Error'Identity);
      end case;
   end Delete;

   procedure Reset (
      File : aliased in out Non_Controlled_File_Type;
      Mode : File_Mode) is
   begin
      Check_File_Open (File);
      declare
         package Holder is
            new Exceptions.Finally.Scoped_Holder (
               Scoped_Handle_And_File,
               Finally);
         Scoped : aliased Scoped_Handle_And_File :=
            (C.winbase.INVALID_HANDLE_VALUE, null);
      begin
         Holder.Assign (Scoped'Access);
         case File.all.Kind is
            when Ordinary =>
               Scoped.Handle := File.Handle;
               Scoped.File := File;
               File := null;
               Flush_Writing_Buffer (Scoped.File);
               --  close explicitly in below
               Scoped.Handle := C.winbase.INVALID_HANDLE_VALUE;
               CloseHandle (Scoped.File.Handle, Raise_On_Error => True);
               Scoped.File.Buffer_Index := 0;
               Scoped.File.Reading_Index := Scoped.File.Buffer_Index;
               Scoped.File.Writing_Index := Scoped.File.Buffer_Index;
               Open_Ordinary (
                  Method => Reset,
                  Handle => Scoped.Handle,
                  Mode => Mode,
                  Name => Scoped.File.Name,
                  Form => Scoped.File.Form);
               Scoped.File.Handle := Scoped.Handle;
               Scoped.File.Mode := Mode;
               if Mode = Append_File then
                  Set_Index_To_Append (Scoped.File);
               end if;
            when Temporary =>
               Scoped.Handle := File.Handle;
               Scoped.File := File;
               File := null;
               Scoped.File.Mode := Mode;
               if Mode = Append_File then
                  Flush_Writing_Buffer (Scoped.File);
                  Set_Index_To_Append (Scoped.File);
               else
                  Set_Index (Scoped.File, 1);
               end if;
            when External | External_No_Close | Standard_Handle =>
               Raise_Exception (Status_Error'Identity);
         end case;
         File := Scoped.File;
         --  complete
         Holder.Clear;
      end;
   end Reset;

   function Mode (File : Non_Controlled_File_Type) return File_Mode is
   begin
      Check_File_Open (File);
      return File.Mode;
   end Mode;

   function Name (File : Non_Controlled_File_Type) return String is
   begin
      Check_File_Open (File);
      return System.Zero_Terminated_WStrings.Value (
         File.Name,
         File.Name_Length);
   end Name;

   function Form (File : Non_Controlled_File_Type) return Packed_Form is
   begin
      Check_File_Open (File);
      return File.Form;
   end Form;

   function Is_Open (File : Non_Controlled_File_Type) return Boolean is
   begin
      return File /= null;
   end Is_Open;

   function End_Of_File (File : Non_Controlled_File_Type) return Boolean is
   begin
      Check_File_Open (File);
      Get_Buffer (File); -- call GetFileType
      if File.Buffer_Length = 0 then -- not disk file
         if File.Reading_Index = File.Buffer_Index then
            declare
               Error : Boolean;
            begin
               Ready_Reading_Buffer (File, Error);
               if Error then
                  Raise_Exception (Use_Error'Identity);
               end if;
            end;
         end if;
         return File.Reading_Index = File.Buffer_Index;
      else
         declare
            Size : C.winnt.LONGLONG;
            Z_Index : C.winnt.LONGLONG;
         begin
            GetFileSizeEx (File.Handle, Size);
            SetFilePointerEx (File.Handle, 0, Z_Index, C.winbase.FILE_CURRENT);
            Z_Index := Z_Index + C.winnt.LONGLONG (Offset_Of_Buffer (File));
            return Z_Index >= Size;
            --  whether writing buffer will expand the file size or not
         end;
      end if;
   end End_Of_File;

   function Stream (File : Non_Controlled_File_Type) return Stream_Access is
      package Conv is
         new System.Address_To_Named_Access_Conversions (
            Root_Stream_Type'Class,
            Stream_Access);
   begin
      Check_File_Open (File);
      if File.Dispatcher.Tag = Tags.No_Tag then
         if not Is_Seekable (File.Handle) then
            File.Dispatcher.Tag := Dispatchers.Root_Dispatcher'Tag;
         else
            File.Dispatcher.Tag := Dispatchers.Seekable_Dispatcher'Tag;
         end if;
         File.Dispatcher.File := File;
      end if;
      return Conv.To_Pointer (File.Dispatcher'Address);
   end Stream;

   procedure Read (
      File : not null Non_Controlled_File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset) is
   begin
      if File.Mode = Out_File then
         Raise_Exception (Mode_Error'Identity);
      end if;
      Last := Item'First - 1;
      if Item'First > Item'Last then
         return;
      end if;
      if File.Reading_Index < File.Buffer_Index then
         Read_From_Buffer (File, Item, Last);
      elsif File.Writing_Index > File.Buffer_Index then
         Flush_Writing_Buffer (File);
      end if;
      if Last < Item'Last then
         Get_Buffer (File);
         declare
            Error : Boolean := False;
            Buffer_Length : constant Stream_Element_Count :=
               File.Buffer_Length;
         begin
            declare
               Taking_Length : Stream_Element_Count;
               Read_Size : aliased C.windef.DWORD;
            begin
               Taking_Length := Item'Last - Last;
               if Buffer_Length > 0 then
                  declare
                     Misaligned : constant Stream_Element_Count :=
                        (Buffer_Length - File.Buffer_Index) rem Buffer_Length;
                  begin
                     if Taking_Length < Misaligned then
                        Taking_Length := 0; -- to use reading buffer
                     else
                        Taking_Length := Taking_Length - Misaligned;
                        Taking_Length := Taking_Length
                           - Taking_Length rem Buffer_Length;
                        Taking_Length := Taking_Length + Misaligned;
                     end if;
                  end;
               end if;
               if Taking_Length > 0 then
                  Error := C.winbase.ReadFile (
                     File.Handle,
                     C.windef.LPVOID (Item (Last + 1)'Address),
                     C.windef.DWORD (Taking_Length),
                     Read_Size'Access,
                     lpOverlapped => null) = 0;
                  if not Error then
                     Last := Last + Stream_Element_Offset (Read_Size);
                     --  update indexes
                     if Buffer_Length > 0 then
                        File.Buffer_Index :=
                           (File.Buffer_Index
                              + Stream_Element_Offset (Read_Size))
                           rem Buffer_Length;
                     else
                        File.Buffer_Index := 0;
                     end if;
                     File.Reading_Index := File.Buffer_Index;
                     File.Writing_Index := File.Buffer_Index;
                  end if;
               end if;
            end;
            if not Error
               and then Last < Item'Last
               and then File.Buffer_Length > 0
            then
               Ready_Reading_Buffer (File, Error); -- reading buffer is empty
               if not Error
                  and then File.Reading_Index < File.Buffer_Index
               then
                  Read_From_Buffer (File, Item (Last + 1 .. Item'Last), Last);
               end if;
            end if;
            if Last < Item'First and then Error then
               Raise_Exception (End_Error'Identity);
            end if;
         end;
      end if;
   end Read;

   procedure Write (
      File : not null Non_Controlled_File_Type;
      Item : Stream_Element_Array)
   is
      First : Stream_Element_Offset;
   begin
      if File.Mode = In_File then
         Raise_Exception (Mode_Error'Identity);
      end if;
      First := Item'First;
      if File.Writing_Index > File.Buffer_Index then
         --  append to writing buffer
         Write_To_Buffer (File, Item, First);
         if File.Writing_Index = File.Buffer_Length then
            Flush_Writing_Buffer (File);
         end if;
      elsif File.Reading_Index < File.Buffer_Index then
         --  reset reading buffer
         Reset_Reading_Buffer (File);
      end if;
      if First <= Item'Last then
         Get_Buffer (File);
         declare
            Buffer_Length : constant Stream_Element_Count :=
               File.Buffer_Length;
         begin
            declare
               Taking_Length : Stream_Element_Count;
               Written : aliased C.windef.DWORD;
            begin
               Taking_Length := Item'Last - First + 1;
               if Buffer_Length > 0 then
                  declare
                     Misaligned : constant Stream_Element_Count :=
                        (Buffer_Length - File.Buffer_Index) rem Buffer_Length;
                  begin
                     if Taking_Length < Misaligned then
                        Taking_Length := 0; -- to use writing buffer
                     else
                        Taking_Length := Taking_Length - Misaligned;
                        Taking_Length := Taking_Length
                           - Taking_Length rem Buffer_Length;
                        Taking_Length := Taking_Length + Misaligned;
                     end if;
                  end;
               end if;
               if Taking_Length > 0 then
                  if C.winbase.WriteFile (
                     File.Handle,
                     C.windef.LPCVOID (Item (First)'Address),
                     C.windef.DWORD (Taking_Length),
                     Written'Access,
                     lpOverlapped => null) = 0
                  then
                     case C.winbase.GetLastError is
                        when C.winerror.ERROR_BROKEN_PIPE
                           | C.winerror.ERROR_NO_DATA =>
                           null;
                        when others =>
                           Raise_Exception (Use_Error'Identity);
                     end case;
                  end if;
                  First := First + Taking_Length;
                  --  update indexes
                  if Buffer_Length > 0 then
                     File.Buffer_Index :=
                        (File.Buffer_Index + Taking_Length) rem Buffer_Length;
                     File.Reading_Index := File.Buffer_Index;
                     File.Writing_Index := File.Buffer_Index;
                  end if;
               end if;
            end;
            if First <= Item'Last and then Buffer_Length > 0 then
               Ready_Writing_Buffer (File);
               Write_To_Buffer (File, Item (First .. Item'Last), First);
            end if;
         end;
      end if;
   end Write;

   procedure Set_Index (
      File : not null Non_Controlled_File_Type;
      To : Stream_Element_Positive_Count)
   is
      Dummy : C.winnt.LONGLONG;
      pragma Unreferenced (Dummy);
      Z_Index : constant Stream_Element_Offset := To - 1; -- zero based
   begin
      Flush_Writing_Buffer (File);
      SetFilePointerEx (
         File.Handle,
         C.winnt.LONGLONG (Z_Index),
         Dummy,
         C.winbase.FILE_BEGIN);
      Set_Buffer_Index (File, Z_Index);
   end Set_Index;

   function Index (File : not null Non_Controlled_File_Type)
      return Stream_Element_Positive_Count
   is
      Result : C.winnt.LONGLONG;
   begin
      SetFilePointerEx (File.Handle, 0, Result, C.winbase.FILE_CURRENT);
      return Stream_Element_Positive_Count (Result + 1)
         + Offset_Of_Buffer (File);
   end Index;

   function Size (File : not null Non_Controlled_File_Type)
      return Stream_Element_Count
   is
      Size : C.winnt.LONGLONG;
   begin
      Flush_Writing_Buffer (File);
      GetFileSizeEx (File.Handle, Size);
      return Count (Size);
   end Size;

   procedure Set_Mode (
      File : aliased in out Non_Controlled_File_Type;
      Mode : File_Mode) is
   begin
      Check_File_Open (File);
      declare
         package Holder is
            new Exceptions.Finally.Scoped_Holder (
               Scoped_Handle_And_File,
               Finally);
         Scoped : aliased Scoped_Handle_And_File :=
            (C.winbase.INVALID_HANDLE_VALUE, null);
         Current : Positive_Count;
      begin
         Holder.Assign (Scoped'Access);
         case File.all.Kind is
            when Ordinary =>
               Scoped.Handle := File.Handle;
               Scoped.File := File;
               File := null;
               Current := Index (Scoped.File);
               Flush_Writing_Buffer (Scoped.File);
               --  close explicitly in below
               Scoped.Handle := C.winbase.INVALID_HANDLE_VALUE;
               CloseHandle (Scoped.File.Handle, Raise_On_Error => True);
               Open_Ordinary (
                  Method => Reset,
                  Handle => Scoped.Handle,
                  Mode => Mode,
                  Name => Scoped.File.Name,
                  Form => Scoped.File.Form);
               Scoped.File.Handle := Scoped.Handle;
               Scoped.File.Mode := Mode;
            when Temporary =>
               Scoped.Handle := File.Handle;
               Scoped.File := File;
               File := null;
               Current := Index (Scoped.File);
               Flush_Writing_Buffer (Scoped.File);
               Scoped.File.Mode := Mode;
            when External | External_No_Close | Standard_Handle =>
               Raise_Exception (Status_Error'Identity);
         end case;
         if Mode = Append_File then
            Set_Index_To_Append (Scoped.File);
         else
            Set_Index (Scoped.File, Current);
         end if;
         File := Scoped.File;
         --  complete
         Holder.Clear;
      end;
   end Set_Mode;

   procedure Flush (File : Non_Controlled_File_Type) is
   begin
      Check_File_Open (File);
      Flush_Writing_Buffer (File);
      if C.winbase.FlushFileBuffers (File.Handle) = 0 then
         --  ERROR_INVALID_HANDLE means fd is not file but terminal, pipe, etc
         if C.winbase.GetLastError /= C.winerror.ERROR_INVALID_HANDLE then
            Raise_Exception (Device_Error'Identity);
         end if;
      end if;
   end Flush;

   --  implementation of handle for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Handle : Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : Packed_Form := Default_Form;
      To_Close : Boolean := False)
   is
      package Name_Holder is
         new Exceptions.Finally.Scoped_Holder (C.winnt.LPWSTR, Finally);
      Kind : Stream_Kind;
      Full_Name : aliased C.winnt.LPWSTR;
      Full_Name_Length : C.size_t;
   begin
      if File /= null then
         Raise_Exception (Status_Error'Identity);
      end if;
      if To_Close then
         Kind := External;
      else
         Kind := External_No_Close;
      end if;
      Full_Name := LPWSTR_Conv.To_Pointer (
         System.Standard_Allocators.Allocate (
            (Name'Length * System.Zero_Terminated_WStrings.Expanding
               + 2) -- '*' & NUL
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      Full_Name_Length := Name'Length + 1;
      Name_Holder.Assign (Full_Name'Access);
      declare
         Full_Name_A : C.winnt.WCHAR_array (C.size_t);
         for Full_Name_A'Address use LPWSTR_Conv.To_Address (Full_Name);
      begin
         Full_Name_A (0) := C.winnt.WCHAR'Val (Wide_Character'Pos ('*'));
         System.Zero_Terminated_WStrings.To_C (Name, Full_Name_A (1)'Access);
      end;
      File := Allocate (
         Handle => Handle,
         Mode => Mode,
         Kind => Kind,
         Name => Full_Name,
         Name_Length => Full_Name_Length,
         Form => Form);
      --  complete
      Name_Holder.Clear;
   end Open;

   function Handle (File : Non_Controlled_File_Type) return Handle_Type is
   begin
      Check_File_Open (File);
      return File.Handle;
   end Handle;

   function Is_Standard (File : Non_Controlled_File_Type) return Boolean is
   begin
      return File /= null and then File.Kind = Standard_Handle;
   end Is_Standard;

end Ada.Streams.Stream_IO.Inside;
