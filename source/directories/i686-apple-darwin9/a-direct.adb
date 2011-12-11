with Ada.Directories.Inside;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Memory;
with System.Native_Time;
with System.Storage_Elements;
with C.errno;
with C.fnmatch;
with C.stdlib;
with C.string;
with C.unistd;
with C.sys.time;
with C.sys.types;
package body Ada.Directories is
   use type System.Storage_Elements.Storage_Offset;
   use type C.char;
   use type C.signed_int; -- ssize_t is signed int or signed long
   use type C.signed_long;
   use type C.size_t;
   use type C.unsigned_char;
   use type C.unsigned_int;
   use type C.dirent.DIR_ptr;
   use type C.sys.dirent.struct_dirent_ptr;
   use type C.sys.types.mode_t;

   --  reference:
   --  http://www.opensource.apple.com/source/gcc/gcc-5664/libiberty/rename.c
   function C_rename (
      zfrom : access constant C.char;
      zto : access constant C.char;
      Overwrite : Boolean)
      return C.signed_int;
   pragma Convention (C, C_rename);
   function C_rename (
      zfrom : access constant C.char;
      zto : access constant C.char;
      Overwrite : Boolean)
      return C.signed_int is
   begin
      if C.unistd.link (zfrom, zto) < 0 then
         if C.errno.errno /= C.errno.EEXIST or else not Overwrite then
            return -1;
         end if;
         --  try to overwrite
         if C.unistd.unlink (zto) < 0
            or else C.unistd.link (zfrom, zto) < 0
         then
            return -1;
         end if;
      end if;
      return C.unistd.unlink (zfrom);
   end C_rename;

   package char_ptr_Conv is new System.Address_To_Named_Access_Conversions (
      C.char,
      C.char_ptr);

   procedure Free is new Unchecked_Deallocation (String, String_Access);

   function To_File_Kind (Attribute : C.sys.types.mode_t) return File_Kind;
   function To_File_Kind (Attribute : C.sys.types.mode_t) return File_Kind is
      Kind_Attr : constant C.sys.types.mode_t :=
         Attribute and C.sys.stat.S_IFMT;
   begin
      if Kind_Attr = C.sys.stat.S_IFDIR then
         return Directory;
      elsif Kind_Attr /= C.sys.stat.S_IFREG then
         return Special_File;
      else
         return Ordinary_File;
      end if;
   end To_File_Kind;

   --  implementation

   procedure Base_Name (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      Simple_Name (Name, First, Last);
      if First > Last or else Name (Last) /= '.' then -- AA-A-16 79.a/2
         for I in reverse First .. Last - 1 loop
            if Name (I) = '.' then
               --  Base_Name (".DOTFILE") = ".DOTFILE"
               if I > First then
                  Last := I - 1;
               end if;
               exit;
            end if;
         end loop;
      end if;
   end Base_Name;

   function Base_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Base_Name (Name, First, Last);
      return Name (First .. Last);
   end Base_Name;

   procedure Check_Assigned (Directory_Entry : Directory_Entry_Type) is
   begin
      if Directory_Entry.Path = null then
         raise Status_Error;
      end if;
   end Check_Assigned;

   function Compose (
      Containing_Directory : String := "";
      Name : String;
      Extension : String := "") return String
   is
      --  if you want to fold '.' or '..', use Hierarchical_File_Names.Compose
      Result : String (
         1 ..
         Containing_Directory'Length + Name'Length + Extension'Length + 2);
      Last : Natural;
   begin
      --  append directory
      Last := Containing_Directory'Length;
      if Last > 0 then
         Result (1 .. Last) := Containing_Directory;
         Include_Trailing_Path_Delimiter (Result, Last);
      end if;
      --  append name
      Result (Last + 1 .. Last + Name'Length) := Name;
      Last := Last + Name'Length;
      --  append extension
      if Extension'Length /= 0 then
         Last := Last + 1;
         Result (Last) := '.';
         Result (Last + 1 .. Last + Extension'Length) := Extension;
         Last := Last + Extension'Length;
      end if;
      return Result (1 .. Last);
   end Compose;

   procedure Containing_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      First := Name'First;
      Last := Name'First - 1;
      for I in reverse Name'Range loop
         case Name (I) is
            when '/' =>
               Last := I; -- no removing root '/'
               Exclude_Trailing_Path_Delimiter (Name, Last);
               exit; -- found
            when others =>
               null;
         end case;
      end loop;
   end Containing_Directory;

   function Containing_Directory (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Containing_Directory (Name, First, Last);
      return Name (First .. Last);
   end Containing_Directory;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Form : String := "";
      Overwrite : Boolean := True)
      renames Inside.Copy_File;

   procedure Create_Directory (New_Directory : String; Form : String := "") is
      pragma Unreferenced (Form);
      Z_New_Directory : constant String := New_Directory & Character'Val (0);
      C_New_Directory : C.char_array (C.size_t);
      for C_New_Directory'Address use Z_New_Directory'Address;
   begin
      if C.sys.stat.mkdir (C_New_Directory (0)'Access, 8#755#) /= 0 then
         case C.errno.errno is
            when C.errno.ENOENT
               | C.errno.ENOTDIR
               | C.errno.ENAMETOOLONG =>
               raise Name_Error;
            when others =>
               raise Use_Error;
         end case;
      end if;
   end Create_Directory;

   procedure Create_Path (New_Directory : String; Form : String := "") is
      pragma Unreferenced (Form);
      First : Positive;
      Created : Natural;
      Last : Positive;
      Step : Boolean;
   begin
      First := New_Directory'First;
      if First <= New_Directory'Last
         and then New_Directory (First) = '/'
      then
         First := First + 1;
      end if;
      Created := First - 1;
      for J in First .. New_Directory'Last loop
         case New_Directory (J) is
            when '/' =>
               Step := True;
               Last := J - 1;
            when others =>
               Step := J = New_Directory'Last;
               Last := J;
         end case;
         if Step then
            if Created < J then
               declare
                  Step_Dir : constant String :=
                     New_Directory (New_Directory'First .. Last);
               begin
                  case Kind (Step_Dir) is
                     when Ordinary_File | Special_File =>
                        raise Use_Error;
                     when Directory =>
                        null;
                  end case;
               exception
                  when Name_Error =>
                     Create_Directory (Step_Dir);
               end;
            end if;
            Created := J;
         end if;
      end loop;
   end Create_Path;

   function Current_Directory return String is
      Path : constant C.char_ptr := C.unistd.getcwd (null, 0);
      Path_Length : constant C.size_t := C.string.strlen (Path);
      Path_String : String (1 .. Natural (Path_Length));
      for Path_String'Address use Path.all'Address;
   begin
      return Result : String := Path_String do -- copy
         pragma Unmodified (Result);
         pragma Unreferenced (Result);
         C.stdlib.free (C.void_ptr (Path.all'Address));
      end return;
   end Current_Directory;

   procedure Delete_Directory (Directory : String) is
      Z_Directory : constant String := Directory & Character'Val (0);
      C_Directory : C.char_array (C.size_t);
      for C_Directory'Address use Z_Directory'Address;
   begin
      if C.unistd.rmdir (C_Directory (0)'Access) < 0 then
         raise Name_Error;
      end if;
   end Delete_Directory;

   procedure Delete_File (Name : String) is
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      if C.unistd.unlink (C_Name (0)'Access) < 0 then
         raise Name_Error;
      end if;
   end Delete_File;

   procedure Delete_Tree (Directory : String) is
      Search : Search_Type;
   begin
      Start_Search (Search, Directory, "*", (others => True));
      while More_Entries (Search) loop
         declare
            Directory_Entry : Directory_Entry_Type;
         begin
            Get_Next_Entry (Search, Directory_Entry);
            declare
               Name : constant String := Full_Name (Directory_Entry);
            begin
               case Kind (Directory_Entry) is
                  when Ordinary_File | Special_File =>
                     Delete_File (Name);
                  when Directories.Directory =>
                     declare
                        Simple : constant String :=
                           Simple_Name (Directory_Entry);
                     begin
                        if Simple /= "." and then Simple /= ".." then
                           Delete_Tree (Name);
                        end if;
                     end;
               end case;
            end;
         end;
      end loop;
      End_Search (Search);
      Delete_Directory (Directory);
   end Delete_Tree;

   procedure Exclude_Trailing_Path_Delimiter (
      S : String;
      Last : in out Natural) is
   begin
      while Last > S'First -- no removing root '/'
         and then S (Last) = '/'
      loop
         Last := Last - 1;
      end loop;
   end Exclude_Trailing_Path_Delimiter;

   function Exists (Name : String) return Boolean is
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
      Attributes : aliased C.sys.stat.struct_stat;
   begin
      return C.sys.stat.lstat (C_Name (0)'Access, Attributes'Access) = 0;
   end Exists;

   procedure Extension (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      First := Name'Last + 1;
      Last := Name'Last;
      for I in reverse Name'Range loop
         case Name (I) is
            when '/' =>
               exit; -- not found
            when '.' =>
               --  Extension (".DOTFILE") = ""
               if I > Name'First and then Name (I - 1) /= '/' then
                  First := I + 1;
               end if;
               exit; -- found
            when others =>
               null;
         end case;
      end loop;
   end Extension;

   function Extension (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Extension (Name, First, Last);
      return Name (First .. Last);
   end Extension;

   procedure Finalize (Search : in out Search_Type) is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      if Search.Handle /= null then
         Dummy := C.dirent.closedir (Search.Handle);
         Search.Handle := null;
         Free (Search.Path);
         System.Memory.Free (char_ptr_Conv.To_Address (Search.Pattern));
      end if;
   end Finalize;

   function Full_Name (Name : String) return String is
   begin
      if Name (Name'First) /= '/' then
         return Compose (Current_Directory, Name);
      else
         return Name;
      end if;
   end Full_Name;

   function Full_Name (Directory_Entry : Directory_Entry_Type) return String is
   begin
      Check_Assigned (Directory_Entry);
      return Compose (
         Directory_Entry.Path.all,
         Simple_Name (Directory_Entry));
   end Full_Name;

   procedure Get_Attributes (
      Name : String;
      Attributes : out C.sys.stat.struct_stat)
   is
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      if C.sys.stat.lstat (
         C_Name (0)'Access,
         Attributes'Unrestricted_Access) < 0
      then
         raise Name_Error;
      end if;
   end Get_Attributes;

   procedure Get_Next_Entry (
      Search : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type) is
   begin
      if Search.Handle = null or else not Search.Has_Next then
         raise Status_Error;
      else
         --  copy entry and get info
         Directory_Entry.Path := Search.Path; -- overwrite
         Directory_Entry.Entry_Data := Search.Data;
         declare
            Z_Name : String := Full_Name (Directory_Entry) & Character'Val (0);
            C_Name : C.char_array (C.size_t);
            for C_Name'Address use Z_Name'Address;
         begin
            if C.sys.stat.lstat (
               C_Name (0)'Access,
               Directory_Entry.State_Data'Access) < 0
            then
               raise Use_Error;
            end if;
         end;
         --  counting
         Search.Count := Search.Count + 1;
         --  search next
         loop
            declare
               Result : aliased C.sys.dirent.struct_dirent_ptr;
            begin
               if C.dirent.readdir_r (
                  Search.Handle,
                  Search.Data'Access,
                  Result'Access) < 0
                  or else Result = null
               then
                  Search.Has_Next := False; -- end
                  exit;
               end if;
            end;
            if Search.Filter (
               To_File_Kind (
                  C.Shift_Left (C.sys.types.mode_t (Search.Data.d_type), 12)))
               and then C.fnmatch.fnmatch (
                  Search.Pattern,
                  Search.Data.d_name (0)'Access, 0) = 0
            then
               exit; -- found
            end if;
         end loop;
      end if;
   end Get_Next_Entry;

   procedure Include_Trailing_Path_Delimiter (
      S : in out String;
      Last : in out Natural) is
   begin
      if S (Last) /= '/' then
         Last := Last + 1;
         S (Last) := '/';
      end if;
   end Include_Trailing_Path_Delimiter;

   function Kind (Name : String) return File_Kind is
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return To_File_Kind (Attributes.st_mode);
   end Kind;

   function Kind (Directory_Entry : Directory_Entry_Type) return File_Kind is
   begin
      Check_Assigned (Directory_Entry);
      return To_File_Kind (Directory_Entry.State_Data.st_mode);
   end Kind;

   function Modification_Time (Name : String) return Calendar.Time is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return Cast (System.Native_Time.To_Time (Attributes.st_mtimespec));
   end Modification_Time;

   function Modification_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
   begin
      Check_Assigned (Directory_Entry);
      return Cast (
         System.Native_Time.To_Time (Directory_Entry.State_Data.st_mtimespec));
   end Modification_Time;

   function More_Entries (Search : Search_Type) return Boolean is
   begin
      return Search.Handle /= null and then Search.Has_Next;
   end More_Entries;

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean := True)
   is
      Z_Old : constant String := Old_Name & Character'Val (0);
      C_Old : C.char_array (C.size_t);
      for C_Old'Address use Z_Old'Address;
      Z_New : constant String := New_Name & Character'Val (0);
      C_New : C.char_array (C.size_t);
      for C_New'Address use Z_New'Address;
   begin
      if C_rename (C_Old (0)'Access, C_New (0)'Access, Overwrite) < 0 then
         case C.errno.errno is
            when C.errno.ENOENT
               | C.errno.ENOTDIR
               | C.errno.EISDIR
               | C.errno.ENAMETOOLONG =>
               raise Name_Error;
            when others =>
               raise Use_Error;
         end case;
      end if;
   end Rename;

   procedure Search (
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True);
      Process : not null access procedure (
         Directory_Entry : Directory_Entry_Type))
   is
      Srch : Search_Type;
      Directory_Entry : Directory_Entry_Type;
   begin
      Start_Search (Srch, Directory, Pattern, Filter);
      while More_Entries (Srch) loop
         Get_Next_Entry (Srch, Directory_Entry);
         Process (Directory_Entry);
      end loop;
      End_Search (Srch);
   end Search;

   procedure Set_Directory (Directory : String) is
      Z_Directory : constant String := Directory & Character'Val (0);
      C_Directory : C.char_array (C.size_t);
      for C_Directory'Address use Z_Directory'Address;
   begin
      if C.unistd.chdir (C_Directory (0)'Access) /= 0 then
         raise Name_Error;
      end if;
   end Set_Directory;

   procedure Set_Modification_Time (Name : String; Time : Calendar.Time) is
      function Cast is new Unchecked_Conversion (Calendar.Time, Duration);
      function To_timeval (X : C.sys.time.struct_timespec)
         return C.sys.time.struct_timeval;
      function To_timeval (X : C.sys.time.struct_timespec)
         return C.sys.time.struct_timeval is
      begin
         return (
            tv_sec => X.tv_sec,
            tv_usec => C.signed_int (X.tv_nsec / 1000));
      end To_timeval;
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
      Attributes : aliased C.sys.stat.struct_stat;
      Times : aliased array (0 .. 1) of aliased C.sys.time.struct_timeval;
   begin
      if C.sys.stat.lstat (C_Name (0)'Access, Attributes'Access) < 0 then
         raise Name_Error;
      end if;
      Times (0) := To_timeval (Attributes.st_atimespec);
      Times (1) := To_timeval (
         System.Native_Time.To_Native_Time (Cast (Time)));
      if C.sys.time.lutimes (C_Name (0)'Access, Times (0)'Access) < 0 then
         raise Use_Error;
      end if;
   end Set_Modification_Time;

   procedure Simple_Name (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      First := Name'First;
      Last := Name'Last;
      for I in reverse Name'Range loop
         case Name (I) is
            when '/' =>
               First := I + 1;
               exit; -- found
            when others =>
               null;
         end case;
      end loop;
   end Simple_Name;

   function Simple_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Simple_Name (Name, First, Last);
      return Name (First .. Last);
   end Simple_Name;

   function Simple_Name (Directory_Entry : Directory_Entry_Type)
      return String is
   begin
      Check_Assigned (Directory_Entry);
      declare
         subtype Result_String is String (
            1 ..
            Natural (Directory_Entry.Entry_Data.d_namlen));
         Result : Result_String;
         for Result'Address use Directory_Entry.Entry_Data.d_name'Address;
      begin
         return Result;
      end;
   end Simple_Name;

   function Size (Name : String) return File_Size is
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      if To_File_Kind (Attributes.st_mode) /= Ordinary_File then
         raise Name_Error;
      else
         return File_Size (Attributes.st_size);
      end if;
   end Size;

   function Size (Directory_Entry : Directory_Entry_Type) return File_Size is
   begin
      if Directory_Entry.Path = null
         or else To_File_Kind (Directory_Entry.State_Data.st_mode) /=
            Ordinary_File
      then
         raise Status_Error;
      else
         return File_Size (Directory_Entry.State_Data.st_size);
      end if;
   end Size;

   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True)) is
   begin
      Finalize (Search); -- cleanup
      declare
         Z_Directory : constant String := Directory & Character'Val (0);
         C_Directory : C.char_array (C.size_t);
         for C_Directory'Address use Z_Directory'Address;
      begin
         Search.Handle := C.dirent.opendir (C_Directory (0)'Access);
      end;
      if Search.Handle = null then
         raise Name_Error;
      else
         Search.Path := new String'(Full_Name (Directory));
         declare
            Length : constant C.size_t := Pattern'Length;
            Term : C.char_ptr;
            Dummy : C.void_ptr;
            pragma Unreferenced (Dummy);
         begin
            Search.Pattern := char_ptr_Conv.To_Pointer (
               System.Memory.Allocate (
                  System.Storage_Elements.Storage_Offset (Length + 1)));
            Dummy := C.string.memcpy (
               C.void_ptr (Search.Pattern.all'Address),
               C.void_const_ptr (Pattern'Address),
               Length);
            Term := char_ptr_Conv.To_Pointer (
               char_ptr_Conv.To_Address (Search.Pattern)
               + System.Storage_Elements.Storage_Offset (Length));
            Term.all := C.char'Val (0);
         end;
         Search.Filter := Filter;
         Search.Count := 0;
         Search.Has_Next := True;
         loop
            declare
               Result : aliased C.sys.dirent.struct_dirent_ptr;
            begin
               if C.dirent.readdir_r (
                  Search.Handle,
                  Search.Data'Access,
                  Result'Access) < 0
                  or else Result = null
               then
                  Search.Has_Next := False; -- end
                  exit;
               end if;
            end;
            if Search.Filter (
               To_File_Kind (C.Shift_Left (
                  C.sys.types.mode_t (Search.Data.d_type), 12)))
               and then C.fnmatch.fnmatch (
                  Search.Pattern,
                  Search.Data.d_name (0)'Access, 0) = 0
               and then (
                  Search.Data.d_name (0) /= '.'
                  or else (
                     Search.Data.d_namlen > 1
                     and then (
                        Search.Data.d_name (1) /= '.'
                        or else Search.Data.d_namlen > 2)))
            then
               exit; -- found
            end if;
         end loop;
      end if;
   end Start_Search;

   function Start_Search (
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True))
      return Search_Type is
   begin
      return Result : Search_Type do
         Start_Search (Result, Directory, Pattern, Filter);
      end return;
   end Start_Search;

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True)
   is
      Z_Source : constant String := Source_Name & Character'Val (0);
      C_Source : C.char_array (C.size_t);
      for C_Source'Address use Z_Source'Address;
      Z_Target : constant String := Target_Name & Character'Val (0);
      C_Target : C.char_array (C.size_t);
      for C_Target'Address use Z_Target'Address;
      path1 : constant not null access constant C.char := C_Source (0)'Access;
      path2 : constant not null access constant C.char := C_Target (0)'Access;
   begin
      if C.unistd.symlink (path1, path2) < 0 then
         if C.errno.errno = C.errno.EEXIST and then Overwrite then
            --  try to overwrite
            if C.unistd.unlink (path2) = 0
               and then C.unistd.symlink (path1, path2) = 0
            then
               return; -- success
            end if;
         end if;
         case C.errno.errno is
            when C.errno.ENOENT
               | C.errno.ENOTDIR
               | C.errno.ENAMETOOLONG =>
               raise Name_Error;
            when others =>
               raise Use_Error;
         end case;
      end if;
   end Symbolic_Link;

   --  iterator

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Search /= null;
   end Has_Element;

   function Iterate (Container : Search_Type) return Iterator is
   begin
      return Container'Unrestricted_Access;
   end Iterate;

   function First (Object : Iterator) return Cursor is
   begin
      if Object.Count /= 0 then
         raise Constraint_Error; -- Status_Error?
      end if;
      return Result : Cursor do
         if More_Entries (Object.all) then
            Result.Search := Object;
            Get_Next_Entry (Object.all, Result.Directory_Entry);
            Result.Index := Object.Count;
         end if;
      end return;
   end First;

   function Next (Object : Iterator; Position : Cursor)
      return Cursor is
   begin
      if Object.Count /= Position.Index then
         raise Constraint_Error; -- Status_Error?
      end if;
      return Result : Cursor do
         if More_Entries (Object.all) then
            Result.Search := Object;
            Get_Next_Entry (Object.all, Result.Directory_Entry);
            Result.Index := Object.Count;
         end if;
      end return;
   end Next;

   function Constant_Reference (Container : Search_Type; Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Directory_Entry'Access);
   end Constant_Reference;

end Ada.Directories;
