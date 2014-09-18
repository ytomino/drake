with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.winerror;
package body Ada.Directory_Searching is
   use Exception_Identification.From_Here;
   use type System.Storage_Elements.Storage_Offset;
   use type C.signed_int;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.winnt.HANDLE; -- C.void_ptr
   use type C.winnt.WCHAR;

   package WIN32_FIND_DATA_ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         C.winbase.WIN32_FIND_DATA,
         C.winbase.struct_WIN32_FIND_DATAW_ptr);

   function Match_Filter (
      Filter : Filter_Type;
      Directory_Entry : not null Directory_Entry_Access)
      return Boolean;
   function Match_Filter (
      Filter : Filter_Type;
      Directory_Entry : not null Directory_Entry_Access)
      return Boolean is
   begin
      return Filter (To_File_Kind (Directory_Entry.dwFileAttributes))
         and then (
            Directory_Entry.cFileName (0) /=
               C.winnt.WCHAR'Val (Wide_Character'Pos ('.'))
            or else (
               Directory_Entry.cFileName (1) /= C.winnt.WCHAR'Val (0)
               and then (
                  Directory_Entry.cFileName (1) /=
                     C.winnt.WCHAR'Val (Wide_Character'Pos ('.'))
                  or else Directory_Entry.cFileName (2) /=
                     C.winnt.WCHAR'Val (0))));
   end Match_Filter;

   --  implementation

   function New_Directory_Entry (Source : not null Directory_Entry_Access)
      return not null Directory_Entry_Access
   is
      Result : Directory_Entry_Access;
   begin
      Result := WIN32_FIND_DATA_ptr_Conv.To_Pointer (
         System.Standard_Allocators.Allocate (
            C.winbase.WIN32_FIND_DATA'Size / Standard'Storage_Unit));
      Result.all := Source.all;
      return Result;
   end New_Directory_Entry;

   procedure Free (X : in out Directory_Entry_Access) is
   begin
      System.Standard_Allocators.Free (
         WIN32_FIND_DATA_ptr_Conv.To_Address (X));
      X := null;
   end Free;

   procedure Start_Search (
      Search : aliased in out Search_Type;
      Directory : String;
      Pattern : String;
      Filter : Filter_Type;
      Directory_Entry : out Directory_Entry_Access;
      Has_Next_Entry : out Boolean) is
   begin
      if Directory'Length = 0 then -- reject
         Raise_Exception (Name_Error'Identity);
      end if;
      declare
         Wildcard : C.winnt.WCHAR_array (
            0 ..
            (Directory'Length + Pattern'Length)
               * System.Zero_Terminated_WStrings.Expanding
               + 1); -- '/'
         Wildcard_Length : C.size_t;
      begin
         --  compose wildcard
         System.Zero_Terminated_WStrings.To_C (
            Directory,
            Wildcard (0)'Access,
            Wildcard_Length);
         case Directory (Directory'Last) is
            when '\' | '/' | ':' =>
               null;
            when others =>
               Wildcard (Wildcard_Length) :=
                  C.winnt.WCHAR'Val (Character'Pos ('\'));
               Wildcard_Length := Wildcard_Length + 1;
         end case;
         System.Zero_Terminated_WStrings.To_C (
            Pattern,
            Wildcard (Wildcard_Length)'Access);
         --  start search
         Directory_Entry := Search.Directory_Entry'Unchecked_Access;
         Search.Handle := C.winbase.FindFirstFileW (
            Wildcard (0)'Access,
            Directory_Entry);
      end;
      if Search.Handle = C.winbase.INVALID_HANDLE_VALUE then
         case C.winbase.GetLastError is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_NO_MORE_FILES =>
               --  no files match the pattern
               Has_Next_Entry := False;
            when others =>
               Raise_Exception (Name_Error'Identity);
         end case;
      else
         Search.Filter := Filter;
         loop
            if Match_Filter (Search.Filter, Directory_Entry) then
               Has_Next_Entry := True;
               exit; -- found
            end if;
            if C.winbase.FindNextFile (Search.Handle, Directory_Entry) = 0 then
               case C.winbase.GetLastError is
                  when C.winerror.ERROR_FILE_NOT_FOUND
                     | C.winerror.ERROR_NO_MORE_FILES =>
                     Has_Next_Entry := False;
                     exit; -- end
                  when others =>
                     Raise_Exception (Use_Error'Identity);
               end case;
            end if;
         end loop;
      end if;
   end Start_Search;

   procedure End_Search (
      Search : aliased in out Search_Type;
      Raise_On_Error : Boolean)
   is
      Handle : constant C.winnt.HANDLE := Search.Handle;
   begin
      Search.Handle := Handle_Type (System.Null_Address);
      if Handle /= C.winbase.INVALID_HANDLE_VALUE then
         if C.winbase.FindClose (Handle) = 0 then
            if Raise_On_Error then
               Raise_Exception (Use_Error'Identity);
            end if;
         end if;
      end if;
   end End_Search;

   procedure Get_Next_Entry (
      Search : aliased in out Search_Type;
      Directory_Entry : out Directory_Entry_Access;
      Has_Next_Entry : out Boolean)
   is
      pragma Unmodified (Search);
   begin
      Directory_Entry := Search.Directory_Entry'Unchecked_Access;
      loop
         if C.winbase.FindNextFile (Search.Handle, Directory_Entry) = 0 then
            case C.winbase.GetLastError is
               when C.winerror.ERROR_FILE_NOT_FOUND
                  | C.winerror.ERROR_NO_MORE_FILES =>
                  Has_Next_Entry := False;
                  exit; -- end
               when others =>
                  Raise_Exception (Use_Error'Identity);
            end case;
         end if;
         if Match_Filter (Search.Filter, Directory_Entry) then
            Has_Next_Entry := True;
            exit; -- found
         end if;
      end loop;
   end Get_Next_Entry;

   function Simple_Name (Directory_Entry : not null Directory_Entry_Access)
      return String is
   begin
      return System.Zero_Terminated_WStrings.Value (
         Directory_Entry.cFileName (0)'Access);
   end Simple_Name;

   function Kind (Directory_Entry : not null Directory_Entry_Access)
      return File_Kind is
   begin
      return To_File_Kind (Directory_Entry.dwFileAttributes);
   end Kind;

   function Size (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Additional : aliased in out Directory_Entry_Additional_Type)
      return Streams.Stream_Element_Count
   is
      pragma Unreferenced (Directory);
      pragma Unreferenced (Additional);
      U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 0,
         LowPart => Directory_Entry.nFileSizeLow,
         HighPart => Directory_Entry.nFileSizeHigh);
   begin
      return Streams.Stream_Element_Count (U.QuadPart);
   end Size;

   function Modification_Time (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Additional : aliased in out Directory_Entry_Additional_Type)
      return System.Native_Time.Native_Time
   is
      pragma Unreferenced (Directory);
      pragma Unreferenced (Additional);
   begin
      return Directory_Entry.ftLastWriteTime;
   end Modification_Time;

   function To_File_Kind (Attributes : C.windef.DWORD) return File_Kind is
   begin
      if (Attributes and C.winnt.FILE_ATTRIBUTE_DIRECTORY) /= 0 then
         return Directory;
      elsif (Attributes and (
         C.winnt.FILE_ATTRIBUTE_DEVICE
         or C.winnt.FILE_ATTRIBUTE_REPARSE_POINT
         or C.winnt.FILE_ATTRIBUTE_VIRTUAL)) = 0
      then
         return Special_File;
      else
         return Ordinary_File;
      end if;
   end To_File_Kind;

end Ada.Directory_Searching;
