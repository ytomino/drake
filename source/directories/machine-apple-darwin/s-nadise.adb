with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.errno;
with C.fnmatch;
with C.stdint;
with C.sys.types;
package body System.Native_Directories.Searching is
   use Ada.Exception_Identification.From_Here;
   use type Storage_Elements.Storage_Offset;
   use type C.char;
   use type C.signed_int;
   use type C.unsigned_char; -- d_namelen in FreeBSD
   use type C.dirent.DIR_ptr;
   use type C.size_t;
   use type C.sys.dirent.struct_dirent_ptr;
   use type C.sys.types.mode_t;

   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   package dirent_ptr_Conv is
      new Address_To_Named_Access_Conversions (
         C.dirent.struct_dirent,
         C.sys.dirent.struct_dirent_ptr);

   procedure memcpy (
      dst : not null C.sys.dirent.struct_dirent_ptr;
      src : not null C.sys.dirent.struct_dirent_ptr;
      n : Storage_Elements.Storage_Count)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memcpy";

   procedure Get_Information (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Information : aliased out C.sys.stat.struct_stat;
      errno : out C.signed_int);
   procedure Get_Information (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Information : aliased out C.sys.stat.struct_stat;
      errno : out C.signed_int)
   is
      S_Length : constant C.size_t := C.size_t (Directory_Entry.d_namlen);
      Full_Name : C.char_array (
         0 ..
         Directory'Length * Zero_Terminated_Strings.Expanding
            + 1 -- '/'
            + S_Length);
      Full_Name_Length : C.size_t;
   begin
      --  compose
      Zero_Terminated_Strings.To_C (
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
      if C.sys.stat.lstat (Full_Name (0)'Access, Information'Access) < 0 then
         errno := C.errno.errno;
      else
         errno := 0;
      end if;
   end Get_Information;

   --  implementation

   function New_Directory_Entry (Source : not null Directory_Entry_Access)
      return not null Directory_Entry_Access
   is
      Result : constant Directory_Entry_Access :=
         dirent_ptr_Conv.To_Pointer (
            Standard_Allocators.Allocate (
               Storage_Elements.Storage_Offset (Source.d_reclen)));
   begin
      memcpy (
         Result,
         Source,
         Storage_Elements.Storage_Offset (Source.d_reclen));
      return Result;
   end New_Directory_Entry;

   procedure Free (X : in out Directory_Entry_Access) is
   begin
      Standard_Allocators.Free (dirent_ptr_Conv.To_Address (X));
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
            Directory'Length * Zero_Terminated_Strings.Expanding);
         Handle : C.dirent.DIR_ptr;
      begin
         Zero_Terminated_Strings.To_C (
            Directory,
            C_Directory (0)'Access);
         Handle := C.dirent.opendir (C_Directory (0)'Access);
         if Handle = null then
            Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
         end if;
         Search.Handle := Handle;
      end;
      Search.Filter := Filter;
      Search.Pattern := char_ptr_Conv.To_Pointer (
         Standard_Allocators.Allocate (
            Storage_Elements.Storage_Offset (Pattern'Length)
               * Zero_Terminated_Strings.Expanding
            + 1)); -- NUL
      Zero_Terminated_Strings.To_C (Pattern, Search.Pattern);
      Get_Next_Entry (Search, Directory_Entry, Has_Next_Entry);
   end Start_Search;

   procedure End_Search (
      Search : aliased in out Search_Type;
      Raise_On_Error : Boolean)
   is
      Handle : constant C.dirent.DIR_ptr := Search.Handle;
   begin
      Search.Handle := null;
      Standard_Allocators.Free (char_ptr_Conv.To_Address (Search.Pattern));
      Search.Pattern := null;
      if C.dirent.closedir (Handle) < 0 and then Raise_On_Error then
         Raise_Exception (IO_Exception_Id (C.errno.errno));
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
         declare
            errno : constant C.signed_int := C.errno.errno;
         begin
            if errno /= 0 then
               Raise_Exception (IO_Exception_Id (errno));
            elsif Directory_Entry = null then
               Has_Next_Entry := False; -- end
               exit;
            end if;
         end;
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

   procedure Get_Entry (
      Directory : String;
      Name : String;
      Directory_Entry : aliased out Directory_Entry_Access;
      Additional : aliased in out Directory_Entry_Additional_Type)
   is
      Dummy_dirent : C.sys.dirent.struct_dirent; -- to use 'Position
      Name_Length : constant C.size_t := Name'Length;
      Record_Length : constant Storage_Elements.Storage_Count :=
         Storage_Elements.Storage_Offset'Max (
            C.sys.dirent.struct_dirent'Size / Standard'Storage_Unit,
            Dummy_dirent.d_name'Position
               + Storage_Elements.Storage_Offset (Name_Length + 1));
      errno : C.signed_int;
   begin
      --  allocation
      Directory_Entry := dirent_ptr_Conv.To_Pointer (
         Standard_Allocators.Allocate (Record_Length));
      --  filling components
--    Directory_Entry.d_seekoff := 0; -- missing in FreeBSD
      Directory_Entry.d_reclen := C.stdint.uint16_t (Record_Length);
      declare
         function To_namlen (X : C.size_t) return C.stdint.uint16_t; -- OSX
         function To_namlen (X : C.size_t) return C.stdint.uint16_t is
         begin
            return C.stdint.uint16_t (X);
         end To_namlen;
         function To_namlen (X : C.size_t) return C.stdint.uint8_t; -- FreeBSD
         function To_namlen (X : C.size_t) return C.stdint.uint8_t is
         begin
            return C.stdint.uint8_t (X);
         end To_namlen;
         pragma Warnings (Off, To_namlen);
      begin
         Directory_Entry.d_namlen := To_namlen (Name_Length);
      end;
      Zero_Terminated_Strings.To_C (Name, Directory_Entry.d_name (0)'Access);
      Get_Information (Directory, Directory_Entry, Additional.Information,
         errno => errno);
      if errno /= 0 then
         Raise_Exception (Named_IO_Exception_Id (errno));
      end if;
      Directory_Entry.d_ino := Additional.Information.st_ino;
      Directory_Entry.d_type := C.stdint.uint8_t (
         C.Shift_Right (Additional.Information.st_mode, 12));
      Additional.Filled := True;
   end Get_Entry;

   function Simple_Name (Directory_Entry : not null Directory_Entry_Access)
      return String is
   begin
      return Zero_Terminated_Strings.Value (
         Directory_Entry.d_name (0)'Access,
         C.size_t (Directory_Entry.d_namlen));
   end Simple_Name;

   function Kind (Directory_Entry : not null Directory_Entry_Access)
      return File_Kind is
   begin
      --  DTTOIF
      return Kind (
         C.Shift_Left (C.sys.types.mode_t (Directory_Entry.d_type), 12));
   end Kind;

   function Size (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Additional : aliased in out Directory_Entry_Additional_Type)
      return Ada.Streams.Stream_Element_Count is
   begin
      if not Additional.Filled then
         Get_Information (Directory, Directory_Entry, Additional.Information);
         Additional.Filled := True;
      end if;
      return Ada.Streams.Stream_Element_Offset (
         Additional.Information.st_size);
   end Size;

   function Modification_Time (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Additional : aliased in out Directory_Entry_Additional_Type)
      return Native_Calendar.Native_Time is
   begin
      if not Additional.Filled then
         Get_Information (Directory, Directory_Entry, Additional.Information);
         Additional.Filled := True;
      end if;
      return Additional.Information.st_mtim;
   end Modification_Time;

   procedure Get_Information (
      Directory : String;
      Directory_Entry : not null Directory_Entry_Access;
      Information : aliased out C.sys.stat.struct_stat)
   is
      errno : C.signed_int;
   begin
      Get_Information (Directory, Directory_Entry, Information,
         errno => errno);
      if errno /= 0 then
         Raise_Exception (IO_Exception_Id (errno));
      end if;
   end Get_Information;

end System.Native_Directories.Searching;
