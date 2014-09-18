with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.errno;
with C.fnmatch;
package body Ada.Directory_Searching is
   use Exception_Identification.From_Here;
   use type System.Storage_Elements.Storage_Offset;
   use type C.char;
   use type C.signed_int;
   use type C.unsigned_char; -- d_namelen in FreeBSD
   use type C.dirent.DIR_ptr;
   use type C.size_t;
   use type C.sys.dirent.struct_dirent_ptr;
   use type C.sys.types.mode_t;

   package char_ptr_Conv is
      new System.Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   package dirent_ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         C.dirent.struct_dirent,
         C.sys.dirent.struct_dirent_ptr);

   procedure memcpy (
      dst : not null C.sys.dirent.struct_dirent_ptr;
      src : not null C.sys.dirent.struct_dirent_ptr;
      n : System.Storage_Elements.Storage_Count);
   pragma Import (Intrinsic, memcpy, "__builtin_memcpy");

   --  implementation

   function New_Directory_Entry (Source : not null Directory_Entry_Access)
      return not null Directory_Entry_Access
   is
      Result : Directory_Entry_Access;
   begin
      Result := dirent_ptr_Conv.To_Pointer (
         System.Standard_Allocators.Allocate (
            System.Storage_Elements.Storage_Count (Source.d_reclen)));
      memcpy (
         Result,
         Source,
         System.Storage_Elements.Storage_Count (Source.d_reclen));
      return Result;
   end New_Directory_Entry;

   procedure Free (X : in out Directory_Entry_Access) is
   begin
      System.Standard_Allocators.Free (dirent_ptr_Conv.To_Address (X));
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
         C_Directory : C.char_array (
            0 ..
            Directory'Length * System.Zero_Terminated_Strings.Expanding);
      begin
         System.Zero_Terminated_Strings.To_C (
            Directory,
            C_Directory (0)'Access);
         Search.Handle := C.dirent.opendir (C_Directory (0)'Access);
      end;
      if Search.Handle = null then
         Raise_Exception (Name_Error'Identity);
      end if;
      Search.Filter := Filter;
      declare
         Pattern_Length : constant Natural := Pattern'Length;
      begin
         Search.Pattern := char_ptr_Conv.To_Pointer (
            System.Standard_Allocators.Allocate (
               System.Storage_Elements.Storage_Offset (
                  Pattern_Length * System.Zero_Terminated_Strings.Expanding)
               + 1)); -- NUL
         System.Zero_Terminated_Strings.To_C (Pattern, Search.Pattern);
      end;
      Get_Next_Entry (Search, Directory_Entry, Has_Next_Entry);
   end Start_Search;

   procedure End_Search (
      Search : aliased in out Search_Type;
      Raise_On_Error : Boolean)
   is
      Handle : constant C.dirent.DIR_ptr := Search.Handle;
   begin
      Search.Handle := null;
      System.Standard_Allocators.Free (
         char_ptr_Conv.To_Address (Search.Pattern));
      Search.Pattern := null;
      if C.dirent.closedir (Handle) < 0 then
         if Raise_On_Error then
            Raise_Exception (Use_Error'Identity);
         end if;
      end if;
   end End_Search;

   procedure Get_Next_Entry (
      Search : aliased in out Search_Type;
      Directory_Entry : out Directory_Entry_Access;
      Has_Next_Entry : out Boolean) is
   begin
      loop
         C.errno.error.all := 0; -- clear errno
         Directory_Entry := C.dirent.readdir (Search.Handle);
         if C.errno.errno /= 0 then
            Raise_Exception (Use_Error'Identity);
         elsif Directory_Entry = null then
            Has_Next_Entry := False; -- end
            exit;
         end if;
         if Search.Filter (Kind (Directory_Entry))
            and then C.fnmatch.fnmatch (
               Search.Pattern,
               Directory_Entry.d_name (0)'Access, 0) = 0
            and then (
               Directory_Entry.d_name (0) /= '.'
               or else (
                  Directory_Entry.d_namlen > 1
                  and then (
                     Directory_Entry.d_name (1) /= '.'
                     or else Directory_Entry.d_namlen > 2)))
         then
            Has_Next_Entry := True;
            exit; -- found
         end if;
      end loop;
   end Get_Next_Entry;

   function Simple_Name (Directory_Entry : not null Directory_Entry_Access)
      return String is
   begin
      return System.Zero_Terminated_Strings.Value (
         Directory_Entry.d_name (0)'Access,
         C.size_t (Directory_Entry.d_namlen));
   end Simple_Name;

   function Kind (Directory_Entry : not null Directory_Entry_Access)
      return File_Kind is
   begin
      --  DTTOIF
      return To_File_Kind (
         C.Shift_Left (C.sys.types.mode_t (Directory_Entry.d_type), 12));
   end Kind;

   function Size (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Additional : aliased in out Directory_Entry_Additional_Type)
      return Streams.Stream_Element_Count is
   begin
      if not Additional.Filled then
         Get_Information (
            Directory,
            Directory_Entry,
            Additional.Information);
         Additional.Filled := True;
      end if;
      return Streams.Stream_Element_Offset (Additional.Information.st_size);
   end Size;

   function Modification_Time (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Additional : aliased in out Directory_Entry_Additional_Type)
      return System.Native_Time.Native_Time is
   begin
      if not Additional.Filled then
         Get_Information (
            Directory,
            Directory_Entry,
            Additional.Information);
         Additional.Filled := True;
      end if;
      return Additional.Information.st_mtim;
   end Modification_Time;

   function To_File_Kind (mode : C.sys.types.mode_t) return File_Kind is
      Masked_Type : constant C.sys.types.mode_t := mode and C.sys.stat.S_IFMT;
   begin
      if Masked_Type = C.sys.stat.S_IFDIR then
         return Directory;
      elsif Masked_Type = C.sys.stat.S_IFREG then
         return Ordinary_File;
      else
         return Special_File;
      end if;
   end To_File_Kind;

   procedure Get_Information (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Information : aliased out C.sys.stat.struct_stat)
   is
      S_Length : constant C.size_t := C.size_t (Directory_Entry.d_namlen);
      Full_Name : C.char_array (
         0 ..
         Directory'Length * System.Zero_Terminated_Strings.Expanding
            + 1 -- '/'
            + S_Length);
      Full_Name_Length : C.size_t;
   begin
      --  compose
      System.Zero_Terminated_Strings.To_C (
         Directory,
         Full_Name (0)'Access,
         Full_Name_Length);
      Full_Name (Full_Name_Length) := '/';
      Full_Name_Length := Full_Name_Length + 1;
      Full_Name (Full_Name_Length .. Full_Name_Length + S_Length - 1) :=
         Directory_Entry.d_name (0 .. S_Length - 1);
      Full_Name_Length := Full_Name_Length + S_Length;
      Full_Name (Full_Name_Length) := C.char'Val (0);
      --  stat
      if C.sys.stat.lstat (
         Full_Name (0)'Access,
         Information'Access) < 0
      then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Get_Information;

end Ada.Directory_Searching;
