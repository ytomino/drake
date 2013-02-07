with Ada.Exceptions;
with System.Zero_Terminated_WStrings;
with C.windef;
with C.winerror;
package body Ada.Directory_Searching is
   use type C.signed_int;
   use type C.windef.DWORD;
   use type C.winnt.HANDLE;
   use type C.winnt.WCHAR;

   function To_File_Kind (Attributes : C.windef.DWORD) return File_Kind;
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

   function Match_Filter (
      Filter : Filter_Type;
      Directory_Entry : not null access Directory_Entry_Type)
      return Boolean;
   function Match_Filter (
      Filter : Filter_Type;
      Directory_Entry : not null access Directory_Entry_Type)
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

   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String;
      Filter : Filter_Type;
      Directory_Entry : not null access Directory_Entry_Type;
      Has_Next_Entry : out Boolean) is
   begin
      declare
         Wildcard : String (1 .. Directory'Length + Pattern'Length + 1);
         Wildcard_Last : Natural;
         W_Wildcard : C.winnt.WCHAR_array (0 .. Wildcard'Length);
      begin
         --  compose wildcard
         Wildcard (1 .. Directory'Length) := Directory;
         case Directory (Directory'Last) is
            when '\' | '/' | ':' =>
               Wildcard (Directory'Length + 1 .. Wildcard'Last - 1) := Pattern;
               Wildcard_Last := Wildcard'Last - 1;
            when others =>
               Wildcard (Directory'Length + 1) := '\';
               Wildcard (Directory'Length + 2 .. Wildcard'Last) := Pattern;
               Wildcard_Last := Wildcard'Last;
         end case;
         --  convert character code
         System.Zero_Terminated_WStrings.Convert (
            Wildcard (1 .. Wildcard_Last),
            W_Wildcard (0)'Access);
         --  start search
         Search.Handle := C.winbase.FindFirstFileW (
            W_Wildcard (0)'Access,
            Directory_Entry);
      end;
      if Search.Handle = C.winbase.INVALID_HANDLE_VALUE then
         case C.winbase.GetLastError is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_NO_MORE_FILES =>
               --  no files match the pattern
               Has_Next_Entry := False;
            when others =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
         end case;
      else
         Search.Filter := Filter;
         loop
            if Match_Filter (Search.Filter, Directory_Entry) then
               Has_Next_Entry := True;
               exit; -- found
            end if;
            if C.winbase.FindNextFileW (
               Search.Handle,
               Directory_Entry) = 0
            then
               Has_Next_Entry := False; -- end
               exit;
            end if;
         end loop;
      end if;
   end Start_Search;

   procedure End_Search (Search : in out Search_Type) is
      Dummy : C.windef.WINBOOL;
      pragma Unreferenced (Dummy);
   begin
      if Search.Handle /= C.winbase.INVALID_HANDLE_VALUE then
         Dummy := C.winbase.FindClose (Search.Handle);
      end if;
      Search.Handle := Handle_Type (System.Null_Address);
   end End_Search;

   procedure Get_Next_Entry (
      Search : in out Search_Type;
      Directory_Entry : not null access Directory_Entry_Type;
      Has_Next_Entry : out Boolean)
   is
      pragma Unmodified (Search);
   begin
      loop
         if C.winbase.FindNextFileW (
            Search.Handle,
            Directory_Entry) = 0
         then
            Has_Next_Entry := False; -- end
            exit;
         end if;
         if Match_Filter (Search.Filter, Directory_Entry) then
            Has_Next_Entry := True;
            exit; -- found
         end if;
      end loop;
   end Get_Next_Entry;

   function Simple_Name (Directory_Entry : Directory_Entry_Type)
      return String is
   begin
      return System.Zero_Terminated_WStrings.Value (
         Directory_Entry.cFileName (0)'Access);
   end Simple_Name;

   procedure Get_Information (
      Directory : String;
      Directory_Entry : Directory_Entry_Type;
      Information : not null access Directory_Entry_Information_Type)
   is
      pragma Unreferenced (Directory);
   begin
      Information.dwFileAttributes := Directory_Entry.dwFileAttributes;
      Information.ftCreationTime := Directory_Entry.ftCreationTime;
      Information.ftLastAccessTime := Directory_Entry.ftLastAccessTime;
      Information.ftLastWriteTime := Directory_Entry.ftLastWriteTime;
      Information.nFileSizeHigh := Directory_Entry.nFileSizeHigh;
      Information.nFileSizeLow := Directory_Entry.nFileSizeLow;
   end Get_Information;

   function Kind (Information : Directory_Entry_Information_Type)
      return File_Kind is
   begin
      return To_File_Kind (Information.dwFileAttributes);
   end Kind;

end Ada.Directory_Searching;
