with Ada.Exceptions;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
with System.Memory;
with C.fnmatch;
with C.sys.dirent;
package body Ada.Directory_Searching is
   use type C.char;
   use type C.signed_int;
   use type C.unsigned_char; -- d_namelen in FreeBSD
   use type C.dirent.DIR_ptr;
   use type C.sys.dirent.struct_dirent_ptr;
   use type C.sys.types.mode_t;

   package char_ptr_Conv is new System.Address_To_Named_Access_Conversions (
      C.char,
      C.char_ptr);

   --  implementation

   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String;
      Filter : Filter_Type;
      Directory_Entry : not null access Directory_Entry_Type;
      Has_Next_Entry : out Boolean) is
   begin
      if Directory'Length = 0 then -- reject
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
      declare
         Z_Directory : constant String := Directory & Character'Val (0);
         C_Directory : C.char_array (C.size_t);
         for C_Directory'Address use Z_Directory'Address;
      begin
         Search.Handle := C.dirent.opendir (C_Directory (0)'Access);
      end;
      if Search.Handle = null then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
      Search.Filter := Filter;
      declare
         Pattern_Length : constant Natural := Pattern'Length;
      begin
         Search.Pattern := char_ptr_Conv.To_Pointer (
            System.Memory.Allocate (
               System.Storage_Elements.Storage_Offset (Pattern_Length + 1)));
         declare
            Search_Pattern : String (Positive);
            for Search_Pattern'Address use Search.Pattern.all'Address;
         begin
            Search_Pattern (1 .. Pattern_Length) := Pattern;
            Search_Pattern (Pattern_Length + 1) := Character'Val (0);
         end;
      end;
      Get_Next_Entry (Search, Directory_Entry, Has_Next_Entry);
   end Start_Search;

   procedure End_Search (Search : in out Search_Type) is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.dirent.closedir (Search.Handle);
      Search.Handle := null;
      System.Memory.Free (char_ptr_Conv.To_Address (Search.Pattern));
      Search.Pattern := null;
   end End_Search;

   procedure Get_Next_Entry (
      Search : in out Search_Type;
      Directory_Entry : not null access Directory_Entry_Type;
      Has_Next_Entry : out Boolean) is
   begin
      loop
         declare
            Result : aliased C.sys.dirent.struct_dirent_ptr;
         begin
            if C.dirent.readdir_r (
               Search.Handle,
               Directory_Entry,
               Result'Access) < 0
               or else Result = null
            then
               Has_Next_Entry := False; -- end
               exit;
            end if;
         end;
         if Search.Filter (Kind (Directory_Entry.all))
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

   function Simple_Name (Directory_Entry : Directory_Entry_Type)
      return String
   is
      subtype Simple_Name_String is String (
         1 ..
         Natural (Directory_Entry.d_namlen));
      Result : Simple_Name_String;
      for Result'Address use Directory_Entry.d_name'Address;
   begin
      return Result;
   end Simple_Name;

   function Kind (Directory_Entry : Directory_Entry_Type)
      return File_Kind is
   begin
      --  DTTOIF
      return To_File_Kind (
         C.Shift_Left (C.sys.types.mode_t (Directory_Entry.d_type), 12));
   end Kind;

   function Size (
      Directory : String;
      Directory_Entry : Directory_Entry_Type;
      Additional : not null access Directory_Entry_Additional_Type)
      return Streams.Stream_Element_Count is
   begin
      if not Additional.Filled then
         Get_Information (
            Directory,
            Directory_Entry,
            Additional.Information'Access);
         Additional.Filled := True;
      end if;
      return Streams.Stream_Element_Offset (Additional.Information.st_size);
   end Size;

   function Modification_Time (
      Directory : String;
      Directory_Entry : Directory_Entry_Type;
      Additional : not null access Directory_Entry_Additional_Type)
      return System.Native_Time.Native_Time is
   begin
      if not Additional.Filled then
         Get_Information (
            Directory,
            Directory_Entry,
            Additional.Information'Access);
         Additional.Filled := True;
      end if;
      return Additional.Information.st_mtimespec;
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
      Directory_Entry : Directory_Entry_Type;
      Information : not null access C.sys.stat.struct_stat)
   is
      subtype Simple_Name_String is String (
         1 ..
         Natural (Directory_Entry.d_namlen));
      Simple_Name : Simple_Name_String;
      for Simple_Name'Address use Directory_Entry.d_name'Address;
      Z_Name : String := Directory & "/" & Simple_Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      if C.sys.stat.lstat (
         C_Name (0)'Access,
         Information) < 0
      then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
   end Get_Information;

end Ada.Directory_Searching;
