with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with C.string;
with C.wincon;
with C.windef;
with C.winerror;
package body System.Native_IO is
   use Ada.Exception_Identification.From_Here;
   use type Ada.IO_Modes.File_Shared;
   use type Ada.IO_Modes.File_Shared_Spec;
   use type Storage_Elements.Storage_Offset;
   use type C.size_t; -- Name_Length
   use type C.windef.DWORD;
   use type C.windef.UINT;
   use type C.windef.WINBOOL;

   package Name_Pointer_Conv is
      new Address_To_Named_Access_Conversions (Name_Character, Name_Pointer);

   Temp_Prefix : constant C.winnt.WCHAR_array (0 .. 3) := (
      C.winnt.WCHAR'Val (Wide_Character'Pos ('A')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('D')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('A')),
      C.winnt.WCHAR'Val (0));

   --  implementation

   procedure Free (Item : in out Name_Pointer) is
   begin
      Standard_Allocators.Free (Name_Pointer_Conv.To_Address (Item));
      Item := null;
   end Free;

   procedure New_Full_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer;
      Out_Length : out Name_Length)
   is
      W_Item : aliased Name_String (
         0 ..
         Item'Length * Zero_Terminated_WStrings.Expanding);
      W_Item_Length : Name_Length;
      Full_Path_Buffer : aliased Name_String (0 .. C.windef.MAX_PATH - 1);
      Full_Path_Pointer : not null access C.winnt.WCHAR :=
         Full_Path_Buffer (0)'Access;
      Full_Path_Length : Name_Length;
   begin
      Zero_Terminated_WStrings.To_C (
         Item,
         W_Item (0)'Access,
         W_Item_Length);
      Full_Path_Length := Name_Length (
         C.winbase.GetFullPathName (
            W_Item (0)'Access,
            Full_Path_Buffer'Length,
            Full_Path_Pointer,
            null));
      if Full_Path_Length = 0 then -- GetFullPathName failed
         Full_Path_Pointer := W_Item (0)'Access;
         Full_Path_Length := W_Item_Length;
      end if;
      --  allocate filename
      Out_Length := Full_Path_Length;
      Out_Item := Name_Pointer_Conv.To_Pointer (
         Standard_Allocators.Allocate (
            Storage_Elements.Storage_Offset (Out_Length + 1) -- NUL
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      declare
         Full_Path_A : Name_String (Name_Length);
         for Full_Path_A'Address use
            Name_Pointer_Conv.To_Address (Full_Path_Pointer);
         Out_Item_A : Name_String (Name_Length);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
      begin
         Out_Item_A (0 .. Out_Length) := Full_Path_A (0 .. Out_Length);
      end;
   end New_Full_Name;

   procedure New_External_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer; -- '*' & Name & NUL
      Out_Length : out Name_Length) is
   begin
      Out_Item := Name_Pointer_Conv.To_Pointer (
         Standard_Allocators.Allocate (
            Storage_Elements.Storage_Offset (
               Item'Length * Zero_Terminated_WStrings.Expanding
               + 2) -- '*' & NUL
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      declare
         Out_Item_A : Name_String (Name_Length);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
      begin
         Out_Item_A (0) := Name_Character'Val (Wide_Character'Pos ('*'));
         Zero_Terminated_WStrings.To_C (
            Item,
            Out_Item_A (1)'Access,
            Out_Length);
      end;
      Out_Length := Out_Length + 1; -- '*'
   end New_External_Name;

   procedure Open_Temporary (
      Handle : aliased out Handle_Type;
      Out_Item : aliased out Name_Pointer;
      Out_Length : out Name_Length)
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
         hTemplateFile => C.winnt.HANDLE (Null_Address));
      if Handle = C.winbase.INVALID_HANDLE_VALUE then
         Raise_Exception (Use_Error'Identity);
      end if;
      --  allocate filename
      Out_Length := C.string.wcslen (Temp_Name (0)'Access);
      Out_Item := Name_Pointer_Conv.To_Pointer (
         Standard_Allocators.Allocate (
            Storage_Elements.Storage_Offset (Out_Length + 1)
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      declare
         Out_Item_A : C.winnt.WCHAR_array (C.size_t);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
      begin
         Out_Item_A (0 .. Out_Length) :=
            Temp_Name (0 .. Out_Length); -- including nul
      end;
   end Open_Temporary;

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : Ada.IO_Modes.File_Mode;
      Name : not null Name_Pointer;
      Form : Packed_Form)
   is
      use type C.winnt.HANDLE;
      DesiredAccess : C.windef.DWORD;
      ShareMode : C.windef.DWORD;
      CreationDisposition : C.windef.DWORD;
      Shared : Ada.IO_Modes.File_Shared;
      Error : C.windef.DWORD;
   begin
      --  Flags, Append_File always has read and write access for Inout_File
      if Form.Shared /= Ada.IO_Modes.By_Mode then
         Shared := Ada.IO_Modes.File_Shared (Form.Shared);
      else
         case Mode is
            when Ada.IO_Modes.In_File =>
               Shared := Ada.IO_Modes.Read_Only;
            when Ada.IO_Modes.Out_File | Ada.IO_Modes.Append_File =>
               Shared := Ada.IO_Modes.Deny;
         end case;
      end if;
      case Method is
         when Create =>
            declare
               use Ada.IO_Modes;
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
               Shared := Ada.IO_Modes.Deny;
               if Form.Overwrite then
                  CreationDisposition := Creations (Mode);
               else
                  CreationDisposition := CREATE_NEW;
               end if;
            end;
         when Open =>
            declare
               use Ada.IO_Modes;
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
               use Ada.IO_Modes;
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
      if Shared /= Ada.IO_Modes.Allow then
         if Form.Wait then
            --  use LockFileEx
            ShareMode := C.winnt.FILE_SHARE_READ or C.winnt.FILE_SHARE_WRITE;
         else
            declare
               Lock_Flags : constant array (
                  Ada.IO_Modes.File_Shared range
                     Ada.IO_Modes.Read_Only ..
                     Ada.IO_Modes.Deny) of
                  C.windef.DWORD := (
                     Ada.IO_Modes.Read_Only => C.winnt.FILE_SHARE_READ,
                     Ada.IO_Modes.Deny => 0);
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
         hTemplateFile => C.winnt.HANDLE (Null_Address));
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
      if Shared /= Ada.IO_Modes.Allow and then Form.Wait then
         declare
            Flags : constant array (
               Ada.IO_Modes.File_Shared range
                  Ada.IO_Modes.Read_Only ..
                  Ada.IO_Modes.Deny) of
               C.windef.DWORD := (
                  Ada.IO_Modes.Read_Only => 0,
                  Ada.IO_Modes.Deny => C.winbase.LOCKFILE_EXCLUSIVE_LOCK);
            Overlapped : aliased C.winbase.OVERLAPPED := (
               0, 0, (0, 0, 0), C.winnt.HANDLE (Null_Address));
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

   procedure Close_Temporary (
      Handle : Handle_Type;
      Name : not null Name_Pointer;
      Raise_On_Error : Boolean)
   is
      pragma Unreferenced (Name);
   begin
      --  CloseHandle remove the temporary file
      Close_Ordinary (Handle, Raise_On_Error);
   end Close_Temporary;

   procedure Close_Ordinary (
      Handle : Handle_Type;
      Raise_On_Error : Boolean)
   is
      Error : Boolean;
   begin
      Error := C.winbase.CloseHandle (Handle) = 0;
      if Error and then Raise_On_Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Close_Ordinary;

   procedure Delete_Ordinary (
      Handle : Handle_Type;
      Name : not null Name_Pointer;
      Raise_On_Error : Boolean)
   is
      Error : Boolean;
   begin
      Error := C.winbase.CloseHandle (Handle) = 0;
      if not Error then
         Error := C.winbase.DeleteFile (Name) = 0;
      end if;
      if Error and then Raise_On_Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Delete_Ordinary;

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

   function Standard_Input return Handle_Type is
   begin
      return C.winbase.GetStdHandle (C.winbase.STD_INPUT_HANDLE);
   end Standard_Input;

   function Standard_Output return Handle_Type is
   begin
      return C.winbase.GetStdHandle (C.winbase.STD_OUTPUT_HANDLE);
   end Standard_Output;

   function Standard_Error return Handle_Type is
   begin
      return C.winbase.GetStdHandle (C.winbase.STD_ERROR_HANDLE);
   end Standard_Error;

   procedure Initialize (
      Standard_Input_Handle : aliased in out Handle_Type;
      Standard_Output_Handle : aliased in out Handle_Type;
      Standard_Error_Handle : aliased in out Handle_Type) is
   begin
      Standard_Input_Handle := Standard_Input;
      Standard_Output_Handle := Standard_Output;
      Standard_Error_Handle := Standard_Error;
   end Initialize;

end System.Native_IO;
