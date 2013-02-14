with Ada.Exceptions;
with System.Address_To_Named_Access_Conversions;
with System.IO_Options;
with System.Memory;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.string;
with C.winbase;
with C.wincon;
with C.windef;
with C.winerror;
package body Ada.Streams.Stream_IO.Inside is
   use type Tags.Tag;
   use type System.Address;
   use type System.Storage_Elements.Storage_Offset;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.UINT;
   use type C.windef.WINBOOL;
   use type C.winnt.LONGLONG;

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
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      NewFilePointer := liNewFilePointer.QuadPart;
   end SetFilePointerEx;

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
      Form : String := "";
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

   --  non-controlled

   package LPWSTR_Conv is
      new System.Address_To_Named_Access_Conversions (
         C.winnt.WCHAR,
         C.winnt.LPWSTR);

   package Non_Controlled_File_Type_Conv is
      new System.Address_To_Named_Access_Conversions (
         Stream_Type,
         Non_Controlled_File_Type);

   function Allocate (
      Handle : Handle_Type;
      Mode : File_Mode;
      Kind : Stream_Kind;
      Name : C.winnt.LPWSTR; -- be freeing on error
      Name_Length : C.signed_int;
      Form : String)
      return Non_Controlled_File_Type;
   function Allocate (
      Handle : Handle_Type;
      Mode : File_Mode;
      Kind : Stream_Kind;
      Name : C.winnt.LPWSTR;
      Name_Length : C.signed_int;
      Form : String)
      return Non_Controlled_File_Type
   is
      Result_Addr : constant System.Address := System.Address (
         C.winbase.HeapAlloc (
            C.winbase.GetProcessHeap,
            0,
            Stream_Type'Size / Standard'Storage_Unit + Form'Length));
   begin
      if Result_Addr = System.Null_Address then
         System.Memory.Free (LPWSTR_Conv.To_Address (Name));
         raise Storage_Error;
      else
         declare
            Result : constant Non_Controlled_File_Type :=
               Non_Controlled_File_Type_Conv.To_Pointer (Result_Addr);
            --  Form is into same memory block
            Result_Form : String (1 .. Form'Length);
            for Result_Form'Address use
               Result_Addr + Stream_Type'Size / Standard'Storage_Unit;
         begin
            Result.Handle := Handle;
            Result.Mode := Mode;
            Result.Kind := Kind;
            Result.Name := Name;
            Result.Name_Length := Name_Length;
            Result.Form := Result_Form'Address;
            Result.Form_Length := Form'Length;
            Result.Buffer := System.Null_Address;
            Result.Buffer_Length := Uninitialized_Buffer;
            Result.Buffer_Index := 0;
            Result.Reading_Index := 0;
            Result.Writing_Index := 0;
            Result.Dispatcher.Tag := Ada.Tags.No_Tag;
            Result.Dispatcher.File := null;
            Result_Form := Form;
            return Result;
         end;
      end if;
   end Allocate;

   procedure Free (File : Non_Controlled_File_Type);
   procedure Free (File : Non_Controlled_File_Type) is
   begin
      if File.Buffer /= File.Buffer_Inline'Address then
         System.Memory.Free (File.Buffer);
      end if;
      System.Memory.Free (LPWSTR_Conv.To_Address (File.Name));
      System.Memory.Free (Non_Controlled_File_Type_Conv.To_Address (File));
   end Free;

   procedure Set_Buffer_Index (
      File : not null Non_Controlled_File_Type;
      Buffer_Index : Stream_Element_Offset);
   procedure Set_Buffer_Index (
      File : not null Non_Controlled_File_Type;
      Buffer_Index : Stream_Element_Offset) is
   begin
      if File.Buffer_Length < Uninitialized_Buffer then
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
      Handle : out Handle_Type;
      Full_Name : out C.winnt.LPWSTR;
      Full_Name_Length : out C.signed_int);
   procedure Open_Temporary_File (
      Handle : out Handle_Type;
      Full_Name : out C.winnt.LPWSTR;
      Full_Name_Length : out C.signed_int)
   is
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
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      --  open
      Handle := C.winbase.CreateFile (
         lpFileName => Temp_Name (0)'Access,
         dwDesiredAccess =>
            C.windef.DWORD'Mod (C.winnt.GENERIC_READ)
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
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      --  allocate filename
      Full_Name_Length := C.signed_int (
         C.string.wcslen (Temp_Name (0)'Access));
      Full_Name := LPWSTR_Conv.To_Pointer (System.Memory.Allocate (
         System.Storage_Elements.Storage_Offset (Full_Name_Length + 1)
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      declare
         W_Full_Name : C.winnt.WCHAR_array (0 .. C.size_t (Full_Name_Length));
         for W_Full_Name'Address use LPWSTR_Conv.To_Address (Full_Name);
      begin
         W_Full_Name := Temp_Name (0 .. C.size_t (Full_Name_Length));
      end;
   end Open_Temporary_File;

   procedure Compose_File_Name (
      Name : String;
      Full_Name : out C.winnt.LPWSTR;
      Full_Name_Length : out C.signed_int);
   procedure Compose_File_Name (
      Name : String;
      Full_Name : out C.winnt.LPWSTR;
      Full_Name_Length : out C.signed_int)
   is
      W_Name : C.winnt.WCHAR_array (0 .. Name'Length);
      W_Name_Length : C.signed_int;
      W_Full_Path : C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
      W_Full_Path_Length : C.windef.DWORD;
   begin
      System.Zero_Terminated_WStrings.Convert (
         Name,
         W_Name (0)'Access,
         W_Name_Length);
      W_Full_Path_Length := C.winbase.GetFullPathName (
         W_Name (0)'Access,
         W_Full_Path'Length,
         W_Full_Path (0)'Access,
         null);
      if W_Full_Path_Length = 0 then
         W_Full_Path_Length := C.windef.DWORD (W_Name_Length);
         W_Full_Path (0 .. C.size_t (W_Full_Path_Length)) :=
            W_Name (0 .. C.size_t (W_Name_Length));
      end if;
      --  allocate filename
      Full_Name_Length := C.signed_int (W_Full_Path_Length);
      Full_Name := LPWSTR_Conv.To_Pointer (System.Memory.Allocate (
         System.Storage_Elements.Storage_Offset (Full_Name_Length + 1)
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      declare
         W_Full_Name : C.winnt.WCHAR_array (0 .. C.size_t (Full_Name_Length));
         for W_Full_Name'Address use LPWSTR_Conv.To_Address (Full_Name);
      begin
         W_Full_Name := W_Full_Path (0 .. C.size_t (Full_Name_Length));
      end;
   end Compose_File_Name;

   function Form_Share_Mode (Form : String; Default : C.windef.DWORD)
      return C.windef.DWORD;
   function Form_Share_Mode (Form : String; Default : C.windef.DWORD)
      return C.windef.DWORD
   is
      First : Positive;
      Last : Natural;
   begin
      System.IO_Options.Form_Parameter (Form, "shared", First, Last);
      if First <= Last and then Form (First) = 'r' then -- read
         return C.winnt.FILE_SHARE_READ;
      elsif First <= Last and then Form (First) = 'w' then -- write
         return 0;
      elsif First <= Last and then Form (First) = 'y' then -- yes
         return C.winnt.FILE_SHARE_READ or C.winnt.FILE_SHARE_WRITE;
      else -- no
         return Default;
      end if;
   end Form_Share_Mode;

   type Open_Method is (Open, Create, Reset);
   pragma Discard_Names (Open_Method);

   procedure Open_Normal (
      Method : Open_Method;
      File : not null Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : not null C.winnt.LPWSTR;
      Form : String);
   procedure Open_Normal (
      Method : Open_Method;
      File : not null Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : not null C.winnt.LPWSTR;
      Form : String)
   is
      Handle : Handle_Type;
      DesiredAccess : C.windef.DWORD;
      ShareMode : C.windef.DWORD;
      CreationDisposition : C.windef.DWORD;
      Error : C.windef.DWORD;
   begin
      --  Flags, Append_File always has read and write access for Inout_File
      if Mode = In_File then
         ShareMode := C.winnt.FILE_SHARE_READ;
      else
         ShareMode := 0;
      end if;
      ShareMode := Form_Share_Mode (Form, ShareMode);
      case Method is
         when Create =>
            declare
               use C.winbase, C.winnt;
               Access_Modes : constant array (File_Mode) of C.windef.DWORD := (
                  In_File =>
                     C.windef.DWORD'Mod (GENERIC_READ) or GENERIC_WRITE,
                  Out_File => GENERIC_WRITE,
                  Append_File =>
                     C.windef.DWORD'Mod (GENERIC_READ) or GENERIC_WRITE);
            begin
               DesiredAccess := Access_Modes (Mode);
               CreationDisposition := CREATE_ALWAYS; -- no truncation
            end;
         when Open =>
            declare
               use C.winbase, C.winnt;
               Access_Modes : constant array (File_Mode) of C.windef.DWORD := (
                  In_File => C.windef.DWORD'Mod (GENERIC_READ),
                  Out_File => GENERIC_WRITE,
                  Append_File =>
                     C.windef.DWORD'Mod (GENERIC_READ) or GENERIC_WRITE);
               Creations : constant array (File_Mode) of C.windef.DWORD := (
                  In_File => OPEN_EXISTING,
                  Out_File => TRUNCATE_EXISTING,
                  Append_File => OPEN_ALWAYS);
            begin
               DesiredAccess := Access_Modes (Mode);
               CreationDisposition := Creations (Mode);
            end;
         when Reset =>
            declare
               use C.winbase, C.winnt;
               Access_Modes : constant array (File_Mode) of C.windef.DWORD := (
                  In_File => C.windef.DWORD'Mod (GENERIC_READ),
                  Out_File => GENERIC_WRITE,
                  Append_File =>
                     C.windef.DWORD'Mod (GENERIC_READ) or GENERIC_WRITE);
               Creations : constant array (File_Mode) of C.windef.DWORD := (
                  In_File => OPEN_EXISTING,
                  Out_File => OPEN_EXISTING, -- no truncation
                  Append_File => OPEN_ALWAYS);
            begin
               DesiredAccess := Access_Modes (Mode);
               CreationDisposition := Creations (Mode);
            end;
      end case;
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
         Free (File); -- free on error
         case Error is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_PATH_NOT_FOUND
               | C.winerror.ERROR_INVALID_NAME
               | C.winerror.ERROR_ALREADY_EXISTS =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
      --  set file
      File.Handle := Handle;
      File.Mode := Mode;
   end Open_Normal;

   procedure Allocate_And_Open (
      Method : Open_Method;
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String);
   procedure Allocate_And_Open (
      Method : Open_Method;
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String)
   is
      Handle : Handle_Type;
      Full_Name : C.winnt.LPWSTR;
      Full_Name_Length : C.signed_int;
   begin
      if Name /= "" then
         Compose_File_Name (Name, Full_Name, Full_Name_Length);
         declare
            New_File : aliased Non_Controlled_File_Type;
         begin
            New_File := Allocate (
               Handle => C.winbase.INVALID_HANDLE_VALUE,
               Mode => Mode,
               Kind => Normal,
               Name => Full_Name,
               Name_Length => Full_Name_Length,
               Form => Form);
            Open_Normal (Method, New_File, Mode, Full_Name, Form);
            File := New_File;
         end;
         if Mode = Append_File then
            Set_Index_To_Append (File); -- sets index to the last
         end if;
      else
         Open_Temporary_File (Handle, Full_Name, Full_Name_Length);
         File := Allocate (
            Handle => Handle,
            Mode => Mode,
            Kind => Temporary,
            Name => Full_Name,
            Name_Length => Full_Name_Length,
            Form => Form);
      end if;
   end Allocate_And_Open;

   procedure Check_File_Open (File : Non_Controlled_File_Type);
   procedure Check_File_Open (File : Non_Controlled_File_Type) is
   begin
      if File = null then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
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
                  Stream_Element_Offset (System.Memory.Page_Size);
            end if;
         end;
         if File.Buffer_Length = 0 then
            File.Buffer := File.Buffer_Inline'Address;
            File.Buffer_Index := 0;
         else
            File.Buffer := System.Memory.Allocate (
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
      Error : out Boolean);
   procedure Flush_Writing_Buffer (
      File : not null Non_Controlled_File_Type;
      Error : out Boolean) is
   begin
      Error := False;
      if File.Writing_Index > File.Buffer_Index then
         declare
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
                     Error := True; -- Device_Error
               end case;
            end if;
         end;
         if not Error then
            File.Buffer_Index := File.Writing_Index rem File.Buffer_Length;
            File.Writing_Index := File.Buffer_Index;
            File.Reading_Index := File.Buffer_Index;
         end if;
      end if;
   end Flush_Writing_Buffer;

   procedure Flush_Writing_Buffer (File : not null Non_Controlled_File_Type);
   procedure Flush_Writing_Buffer (File : not null Non_Controlled_File_Type) is
      Error : Boolean;
   begin
      Flush_Writing_Buffer (File, Error);
      if Error then
         Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
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

   procedure Close_File (
      File : Non_Controlled_File_Type;
      Raise_On_Error : Boolean);
   procedure Close_File (
      File : Non_Controlled_File_Type;
      Raise_On_Error : Boolean)
   is
      Error : Boolean;
   begin
      if File.Kind /= Temporary then
         Flush_Writing_Buffer (File, Error);
         if Error and then Raise_On_Error then
            Free (File); -- free on error
            Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
         end if;
      end if;
      case File.Kind is
         when Normal | Temporary | External =>
            Error := C.winbase.CloseHandle (File.Handle) = 0;
            --  CloseHandle remove the temporary file
            if Error and then Raise_On_Error then
               Free (File); -- free on error
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
            end if;
         when External_No_Close | Standard_Handle =>
            null;
      end case;
   end Close_File;

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
      Form : String := "") is
   begin
      if File /= null then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
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
      Form : String := "") is
   begin
      if File /= null then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Allocate_And_Open (
         Method => Open,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
   end Open;

   procedure Close (
      File : in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True) is
   begin
      Check_File_Open (File);
      declare
         Freeing_File : constant not null Non_Controlled_File_Type := File;
         Kind : constant Stream_Kind := File.Kind;
      begin
         File := null;
         Close_File (Freeing_File, Raise_On_Error);
         case Kind is
            when Normal | Temporary | External | External_No_Close =>
               Free (Freeing_File);
            when Standard_Handle =>
               null; -- statically allocated
         end case;
      end;
   end Close;

   procedure Delete (File : in out Non_Controlled_File_Type) is
   begin
      Check_File_Open (File);
      case File.Kind is
         when Normal =>
            declare
               Deleting_File_Name : C.winnt.WCHAR_array (
                  0 ..
                  C.size_t (File.Name_Length));
            begin
               declare
                  File_Name : C.winnt.WCHAR_array (
                     0 ..
                     C.size_t (File.Name_Length));
                  for File_Name'Address use LPWSTR_Conv.To_Address (File.Name);
               begin
                  Deleting_File_Name := File_Name;
               end;
               Close (File, Raise_On_Error => True);
               if C.winbase.DeleteFile (Deleting_File_Name (0)'Access) = 0 then
                  Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
               end if;
            end;
         when Temporary =>
            Close (File, Raise_On_Error => True);
         when External | External_No_Close | Standard_Handle =>
            Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end case;
   end Delete;

   procedure Reset (
      File : not null access Non_Controlled_File_Type;
      Mode : File_Mode) is
   begin
      Check_File_Open (File.all);
      case File.all.Kind is
         when Normal =>
            declare
               File2 : constant Non_Controlled_File_Type := File.all;
               Form : String (1 .. File2.Form_Length);
               for Form'Address use File2.Form;
            begin
               File.all := null;
               Close_File (File2, Raise_On_Error => True);
               File2.Buffer_Index := 0;
               File2.Reading_Index := File2.Buffer_Index;
               File2.Writing_Index := File2.Buffer_Index;
               Open_Normal (
                  Method => Reset,
                  File => File2,
                  Mode => Mode,
                  Name => File2.Name,
                  Form => Form);
               File.all := File2;
            end;
         when Temporary =>
            File.all.Mode := Mode;
            Set_Index (File.all, 1);
         when External | External_No_Close | Standard_Handle =>
            Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end case;
      if Mode = Append_File then
         Set_Index_To_Append (File.all);
      end if;
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

   function Form (File : Non_Controlled_File_Type) return String is
   begin
      Check_File_Open (File);
      declare
         A_Form : String (1 .. File.Form_Length);
         for A_Form'Address use File.Form;
      begin
         return A_Form;
      end;
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
                  Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
               end if;
            end;
         end if;
         return File.Reading_Index = File.Buffer_Index;
      else
         declare
            Size : aliased C.winnt.LARGE_INTEGER;
            Z_Index : C.winnt.LONGLONG;
         begin
            if C.winbase.GetFileSizeEx (File.Handle, Size'Access) = 0 then
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
            end if;
            SetFilePointerEx (File.Handle, 0, Z_Index, C.winbase.FILE_CURRENT);
            Z_Index := Z_Index + C.winnt.LONGLONG (Offset_Of_Buffer (File));
            return Z_Index >= Size.QuadPart;
            --  whether writing buffer will expand the file size or not
         end;
      end if;
   end End_Of_File;

   function Stream (File : Non_Controlled_File_Type) return Stream_Access is
      package Conv is new System.Address_To_Named_Access_Conversions (
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
         Exceptions.Raise_Exception_From_Here (Mode_Error'Identity);
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
               Exceptions.Raise_Exception_From_Here (End_Error'Identity);
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
         Exceptions.Raise_Exception_From_Here (Mode_Error'Identity);
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
                           Exceptions.Raise_Exception_From_Here (
                              Use_Error'Identity);
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
      Size : aliased C.winnt.LARGE_INTEGER;
   begin
      Flush_Writing_Buffer (File);
      if C.winbase.GetFileSizeEx (File.Handle, Size'Access) = 0 then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      return Count (Size.QuadPart);
   end Size;

   procedure Set_Mode (
      File : not null access Non_Controlled_File_Type;
      Mode : File_Mode)
   is
      Current : Positive_Count;
   begin
      Check_File_Open (File.all);
      Current := Index (File.all);
      case File.all.Kind is
         when Normal =>
            declare
               File2 : constant Non_Controlled_File_Type := File.all;
               Form : String (1 .. File2.Form_Length);
               for Form'Address use File2.Form;
            begin
               File.all := null;
               Close_File (File2, Raise_On_Error => True);
               Open_Normal (
                  Method => Reset,
                  File => File2,
                  Mode => Mode,
                  Name => File2.Name,
                  Form => Form);
               File.all := File2;
            end;
         when Temporary =>
            Flush_Writing_Buffer (File.all);
            File.all.Mode := Mode;
         when External | External_No_Close | Standard_Handle =>
            Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end case;
      if Mode = Append_File then
         Set_Index_To_Append (File.all);
      else
         Set_Index (File.all, Current);
      end if;
   end Set_Mode;

   procedure Flush (File : Non_Controlled_File_Type) is
   begin
      Check_File_Open (File);
      Flush_Writing_Buffer (File);
      if C.winbase.FlushFileBuffers (File.Handle) = 0 then
         --  ERROR_INVALID_HANDLE means fd is not file but terminal, pipe, etc
         if C.winbase.GetLastError /= C.winerror.ERROR_INVALID_HANDLE then
            Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
         end if;
      end if;
   end Flush;

   --  handle for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Handle : Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "";
      To_Close : Boolean := False)
   is
      Kind : Stream_Kind;
      Full_Name : C.winnt.LPWSTR;
      Full_Name_Length : C.signed_int;
   begin
      if File /= null then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      if To_Close then
         Kind := External;
      else
         Kind := External_No_Close;
      end if;
      Full_Name := LPWSTR_Conv.To_Pointer (
         System.Memory.Allocate (
            (Name'Length + 2) * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      Full_Name_Length := Name'Length + 1;
      declare
         Full_Name_A : C.winnt.WCHAR_array (C.size_t);
         for Full_Name_A'Address use LPWSTR_Conv.To_Address (Full_Name);
      begin
         Full_Name_A (0) := C.winnt.WCHAR'Val (Wide_Character'Pos ('*'));
         System.Zero_Terminated_WStrings.Convert (
            Name,
            Full_Name_A (1)'Access);
      end;
      File := Allocate (
         Handle => Handle,
         Mode => Mode,
         Kind => Kind,
         Name => Full_Name,
         Name_Length => Full_Name_Length,
         Form => Form);
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
