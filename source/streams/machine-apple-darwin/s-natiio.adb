with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with C.errno;
with C.fcntl;
with C.stdlib;
with C.sys.file;
with C.sys.mman;
with C.sys.stat;
with C.sys.types;
package body System.Native_IO is
   use Ada.Exception_Identification.From_Here;
   use type Ada.IO_Modes.File_Shared;
   use type Ada.IO_Modes.File_Shared_Spec;
   use type Ada.Streams.Stream_Element_Offset;
   use type C.char; -- Name_Character
   use type C.char_array; -- Name_String
   use type C.char_ptr; -- Name_Pointer
   use type C.size_t; -- Name_Length
   use type C.unsigned_int;
   use type C.unsigned_short;
   use type C.sys.types.off_t;

   pragma Compile_Time_Error (C.sys.types.off_t'Size /= 64,
      "off_t is not 64bit");

   function strlen (Item : not null access constant C.char) return C.size_t
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_strlen";

   package Name_Pointer_Conv is
      new Address_To_Named_Access_Conversions (Name_Character, Name_Pointer);

   Temp_Variable : constant C.char_array := "TMPDIR" & C.char'Val (0);
   Temp_Template : constant C.char_array := "ADAXXXXXX" & C.char'Val (0);

   --  implementation

   procedure Free (Item : in out Name_Pointer) is
   begin
      C.stdlib.free (C.void_ptr (Name_Pointer_Conv.To_Address (Item)));
      Item := null;
   end Free;

   procedure New_Full_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer;
      Out_Length : out Name_Length)
   is
      Item_Length : constant Natural := Item'Length;
   begin
      if Item (Item'First) = '/' then
         --  absolute path
         Out_Item := Name_Pointer_Conv.To_Pointer (
            Address (
               C.stdlib.malloc (
                  C.size_t (Item_Length) * Zero_Terminated_Strings.Expanding
                  + 1))); -- NUL
         if Out_Item = null then
            raise Storage_Error;
         end if;
         Out_Length := 0;
      else
         --  current directory
         Out_Item := C.unistd.getcwd (null, 0);
         Out_Length := strlen (Out_Item);
         --  reuse the memory from malloc (similar to reallocf)
         declare
            New_Out_Item : constant Name_Pointer :=
               Name_Pointer_Conv.To_Pointer (
                  Address (
                     C.stdlib.realloc (
                        C.void_ptr (Name_Pointer_Conv.To_Address (Out_Item)),
                        Out_Length
                           + C.size_t (Item_Length)
                              * Zero_Terminated_Strings.Expanding
                           + 2))); -- '/' & NUL
         begin
            if New_Out_Item = null then
               raise Storage_Error;
            end if;
            Out_Item := New_Out_Item;
         end;
         --  append slash
         declare
            Out_Item_A : Name_String (Name_Length);
            for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
         begin
            if Out_Item_A (Out_Length - 1) /= '/' then
               Out_Item_A (Out_Length) := '/';
               Out_Length := Out_Length + 1;
            end if;
         end;
      end if;
      --  append Item
      declare
         Out_Item_A : Name_String (Name_Length);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
         Appended_Name_Length : Name_Length;
      begin
         Zero_Terminated_Strings.To_C (
            Item,
            Out_Item_A (Out_Length)'Access,
            Appended_Name_Length);
         Out_Length := Out_Length + Appended_Name_Length;
      end;
   end New_Full_Name;

   procedure New_External_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer;
      Out_Length : out Name_Length) is
   begin
      Out_Item := Name_Pointer_Conv.To_Pointer (
         Address (
            C.stdlib.malloc (
               Item'Length * Zero_Terminated_Strings.Expanding
               + 2))); -- '*' & NUL
      if Out_Item = null then
         raise Storage_Error;
      end if;
      declare
         Out_Item_A : Name_String (Name_Length);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
      begin
         Out_Item_A (0) := '*';
         Zero_Terminated_Strings.To_C (
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
      Temp_Template_Length : constant C.size_t := Temp_Template'Length - 1;
      Temp_Dir : C.char_ptr;
   begin
      --  compose template
      Temp_Dir := C.stdlib.getenv (Temp_Variable (0)'Access);
      if Temp_Dir /= null then
         --  environment variable TMPDIR
         Out_Length := strlen (Temp_Dir);
         Out_Item := Name_Pointer_Conv.To_Pointer (
            Address (
               C.stdlib.malloc (
                  Out_Length + Temp_Template_Length + 2))); -- '/' & NUL
         if Out_Item = null then
            raise Storage_Error;
         end if;
         declare
            Temp_Dir_A : C.char_array (C.size_t);
            for Temp_Dir_A'Address use Name_Pointer_Conv.To_Address (Temp_Dir);
            Out_Item_A : C.char_array (C.size_t);
            for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
         begin
            Out_Item_A (0 .. Out_Length - 1) :=
               Temp_Dir_A (0 .. Out_Length - 1);
         end;
      else
         --  current directory
         Out_Item := C.unistd.getcwd (null, 0);
         Out_Length := strlen (Out_Item);
         --  reuse the memory from malloc (similar to reallocf)
         declare
            New_Out_Item : constant C.char_ptr :=
               Name_Pointer_Conv.To_Pointer (
                  Address (
                     C.stdlib.realloc (
                        C.void_ptr (Name_Pointer_Conv.To_Address (Out_Item)),
                        Out_Length + Temp_Template_Length + 2))); -- '/' & NUL
         begin
            if New_Out_Item = null then
               raise Storage_Error;
            end if;
            Out_Item := New_Out_Item;
         end;
      end if;
      declare
         Out_Item_A : C.char_array (C.size_t);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
      begin
         --  append slash
         if Out_Item_A (Out_Length - 1) /= '/' then
            Out_Item_A (Out_Length) := '/';
            Out_Length := Out_Length + 1;
         end if;
         --  append template
         Out_Item_A (Out_Length .. Out_Length + Temp_Template_Length) :=
            Temp_Template; -- including nul
         Out_Length := Out_Length + Temp_Template_Length;
      end;
      --  open
      declare
         use C.stdlib; -- Linux, POSIX.1-2008
         use C.unistd; -- Darwin, FreeBSD
      begin
         Handle := mkstemp (Out_Item);
      end;
      if Handle < 0 then
         Raise_Exception (IO_Exception_Id (C.errno.errno));
      end if;
      Set_Close_On_Exec (Handle);
   end Open_Temporary;

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : Ada.IO_Modes.File_Mode;
      Name : not null Name_Pointer;
      Form : Packed_Form)
   is
      Flags : C.unsigned_int;
      Modes : constant := 8#644#;
      Shared : Ada.IO_Modes.File_Shared;
      errno : C.signed_int;
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
               use C.fcntl;
               Table : constant array (File_Mode) of C.unsigned_int := (
                  In_File => O_RDWR or O_CREAT or O_TRUNC,
                  Out_File => O_WRONLY or O_CREAT or O_TRUNC,
                  Append_File => O_RDWR or O_CREAT); -- no truncation
            begin
               Flags := Table (Mode);
               Shared := Deny;
               if not Form.Overwrite then
                  Flags := Flags or O_EXCL;
               end if;
            end;
         when Open =>
            declare
               use Ada.IO_Modes;
               use C.fcntl;
               Table : constant array (File_Mode) of C.unsigned_int := (
                  In_File => O_RDONLY,
                  Out_File => O_WRONLY or O_TRUNC,
                  Append_File => O_RDWR); -- O_APPEND ignores lseek
            begin
               Flags := Table (Mode);
            end;
         when Reset =>
            declare
               use Ada.IO_Modes;
               use C.fcntl;
               Table : constant array (File_Mode) of C.unsigned_int := (
                  In_File => O_RDONLY,
                  Out_File => O_WRONLY,
                  Append_File => O_RDWR); -- O_APPEND ignores lseek
            begin
               Flags := Table (Mode);
            end;
      end case;
      if Shared /= Ada.IO_Modes.Allow then
         if Form.Wait then
            declare
               Lock_Flags : constant array (
                  Ada.IO_Modes.File_Shared range
                     Ada.IO_Modes.Read_Only ..
                     Ada.IO_Modes.Deny) of
                  C.unsigned_int := (
                     Ada.IO_Modes.Read_Only => C.fcntl.O_SHLOCK,
                     Ada.IO_Modes.Deny => C.fcntl.O_EXLOCK);
            begin
               Flags := Flags or Lock_Flags (Shared);
            end;
         else
            null; -- use flock
         end if;
      else
         null; -- use flock
      end if;
      Flags := Flags or C.fcntl.O_CLOEXEC;
      --  open
      Handle := C.fcntl.open (Name, C.signed_int (Flags), Modes);
      if Handle < 0 then
         errno := C.errno.errno;
         case errno is
            when C.errno.ENOTDIR
               | C.errno.ENAMETOOLONG
               | C.errno.ENOENT
               | C.errno.EEXIST -- O_EXCL
               | C.errno.EISDIR =>
               Raise_Exception (Name_Error'Identity);
            when others =>
               Raise_Exception (IO_Exception_Id (errno));
         end case;
      end if;
      declare
         O_CLOEXEC_Is_Missing : constant Boolean :=
            C.fcntl.O_CLOEXEC = 0;
         pragma Warnings (Off, O_CLOEXEC_Is_Missing);
      begin
         if O_CLOEXEC_Is_Missing then
            --  set FD_CLOEXEC if O_CLOEXEC is missing
            Set_Close_On_Exec (Handle);
         end if;
      end;
      declare
         O_EXLOCK_Is_Missing : constant Boolean :=
            C.fcntl.O_EXLOCK = 0;
         pragma Warnings (Off, O_EXLOCK_Is_Missing);
         Race_Is_Raising : constant Boolean := not Form.Wait;
         pragma Warnings (Off, Race_Is_Raising);
         Operation_Table : constant array (
            Ada.IO_Modes.File_Shared range
               Ada.IO_Modes.Read_Only ..
               Ada.IO_Modes.Deny) of
            C.unsigned_int := (
               Ada.IO_Modes.Read_Only => C.sys.file.LOCK_SH,
               Ada.IO_Modes.Deny => C.sys.file.LOCK_EX);
         operation : C.unsigned_int;
      begin
         if Shared /= Ada.IO_Modes.Allow
            and then (O_EXLOCK_Is_Missing or else Race_Is_Raising)
         then
            operation := Operation_Table (Shared);
            if Race_Is_Raising then
               operation := operation or C.sys.file.LOCK_NB;
            end if;
            if C.sys.file.flock (Handle, C.signed_int (operation)) < 0 then
               errno := C.errno.errno;
               case errno is
                  when C.errno.EWOULDBLOCK =>
                     Raise_Exception (Tasking_Error'Identity);
                     --  Is Tasking_Error suitable?
                  when others =>
                     Raise_Exception (Use_Error'Identity);
               end case;
            end if;
         end if;
      end;
   end Open_Ordinary;

   procedure Close_Ordinary (
      Handle : Handle_Type;
      Name : not null Name_Pointer;
      Raise_On_Error : Boolean)
   is
      pragma Unreferenced (Name);
      Error : Boolean;
   begin
      Error := C.unistd.close (Handle) < 0;
      if Error and then Raise_On_Error then
         Raise_Exception (IO_Exception_Id (C.errno.errno));
      end if;
   end Close_Ordinary;

   procedure Delete_Ordinary (
      Handle : Handle_Type;
      Name : not null Name_Pointer;
      Raise_On_Error : Boolean)
   is
      Error : Boolean;
   begin
      Error := C.unistd.close (Handle) < 0;
      if not Error then
         Error := C.unistd.unlink (Name) < 0;
      end if;
      if Error and then Raise_On_Error then
         Raise_Exception (IO_Exception_Id (C.errno.errno));
      end if;
   end Delete_Ordinary;

   procedure Set_Close_On_Exec (Handle : Handle_Type) is
      Error : Boolean;
   begin
      Error := C.fcntl.fcntl (
         Handle,
         C.fcntl.F_SETFD,
         C.fcntl.FD_CLOEXEC) < 0;
      if Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Set_Close_On_Exec;

   function Is_Terminal (Handle : Handle_Type) return Boolean is
   begin
      return C.unistd.isatty (Handle) /= 0;
   end Is_Terminal;

   function Is_Seekable (Handle : Handle_Type) return Boolean is
   begin
      return C.unistd.lseek (
         Handle,
         0,
         C.unistd.SEEK_CUR) >= 0;
   end Is_Seekable;

   function Block_Size (Handle : Handle_Type)
      return Ada.Streams.Stream_Element_Count
   is
      Result : Ada.Streams.Stream_Element_Count;
   begin
      if Is_Terminal (Handle) then
         Result := 0; -- no buffering for terminal
      else
         declare
            Info : aliased C.sys.stat.struct_stat;
            File_Type : C.sys.types.mode_t;
         begin
            if C.sys.stat.fstat (Handle, Info'Access) < 0 then
               Raise_Exception (IO_Exception_Id (C.errno.errno));
            end if;
            File_Type := Info.st_mode and C.sys.stat.S_IFMT;
            if File_Type = C.sys.stat.S_IFIFO
               or else File_Type = C.sys.stat.S_IFSOCK
            then
               Result := 0; -- no buffering for pipe and socket
            else
               --  disk file
               Result := Ada.Streams.Stream_Element_Offset'Max (
                  2, -- Buffer_Length /= 1
                  Ada.Streams.Stream_Element_Offset (Info.st_blksize));
            end if;
         end;
      end if;
      return Result;
   end Block_Size;

   procedure Read (
      Handle : Handle_Type;
      Item : Address;
      Length : Ada.Streams.Stream_Element_Offset;
      Out_Length : out Ada.Streams.Stream_Element_Offset)
   is
      Read_Size : C.sys.types.ssize_t;
   begin
      Read_Size := C.unistd.read (
         Handle,
         C.void_ptr (Item),
         C.size_t (Length));
      Out_Length := Ada.Streams.Stream_Element_Offset (Read_Size);
   end Read;

   procedure Write (
      Handle : Handle_Type;
      Item : Address;
      Length : Ada.Streams.Stream_Element_Offset;
      Out_Length : out Ada.Streams.Stream_Element_Offset)
   is
      Written_Size : C.sys.types.ssize_t;
   begin
      Written_Size := C.unistd.write (
         Handle,
         C.void_const_ptr (Item),
         C.size_t (Length));
      Out_Length := Ada.Streams.Stream_Element_Offset (Written_Size);
      if Out_Length < 0 then
         case C.errno.errno is
            when C.errno.EPIPE =>
               Out_Length := 0;
            when others =>
               null;
         end case;
      end if;
   end Write;

   procedure Flush (Handle : Handle_Type) is
   begin
      if C.unistd.fsync (Handle) < 0 then
         case C.errno.errno is
            when C.errno.EINVAL =>
               null; -- means fd is not file but FIFO, etc.
            when others =>
               Raise_Exception (Device_Error'Identity);
         end case;
      end if;
   end Flush;

   procedure Set_Relative_Index (
      Handle : Handle_Type;
      Relative_To : Ada.Streams.Stream_Element_Offset;
      Whence : C.signed_int;
      New_Index : out Ada.Streams.Stream_Element_Offset)
   is
      Offset : C.sys.types.off_t;
   begin
      Offset := C.unistd.lseek (
         Handle,
         C.sys.types.off_t (Relative_To),
         Whence);
      if Offset < 0 then
         Raise_Exception (IO_Exception_Id (C.errno.errno));
      end if;
      New_Index := Ada.Streams.Stream_Element_Offset (Offset) + 1;
   end Set_Relative_Index;

   function Index (Handle : Handle_Type)
      return Ada.Streams.Stream_Element_Offset
   is
      Offset : C.sys.types.off_t;
   begin
      Offset := C.unistd.lseek (Handle, 0, C.unistd.SEEK_CUR);
      if Offset < 0 then
         Raise_Exception (IO_Exception_Id (C.errno.errno));
      end if;
      return Ada.Streams.Stream_Element_Offset (Offset) + 1;
   end Index;

   function Size (Handle : Handle_Type)
      return Ada.Streams.Stream_Element_Count
   is
      Info : aliased C.sys.stat.struct_stat;
   begin
      if C.sys.stat.fstat (Handle, Info'Access) < 0 then
         Raise_Exception (IO_Exception_Id (C.errno.errno));
      end if;
      return Ada.Streams.Stream_Element_Offset (Info.st_size);
   end Size;

   procedure Open_Pipe (
      Reading_Handle : aliased out Handle_Type;
      Writing_Handle : aliased out Handle_Type)
   is
      Handles : aliased C.signed_int_array (0 .. 1);
   begin
      if C.unistd.pipe (Handles (0)'Access) < 0 then
         Raise_Exception (Use_Error'Identity);
      else
         Set_Close_On_Exec (Handles (0));
         Set_Close_On_Exec (Handles (1));
         Reading_Handle := Handles (0);
         Writing_Handle := Handles (1);
      end if;
   end Open_Pipe;

   procedure Map (
      Mapping : out Mapping_Type;
      Handle : Handle_Type;
      Offset : Ada.Streams.Stream_Element_Offset;
      Size : Ada.Streams.Stream_Element_Count;
      Writable : Boolean)
   is
      Protects : constant array (Boolean) of C.signed_int := (
         C.sys.mman.PROT_READ,
         C.sys.mman.PROT_READ + C.sys.mman.PROT_WRITE);
      Mapped_Offset : constant C.sys.types.off_t :=
         C.sys.types.off_t (Offset) - 1;
      Mapped_Size : constant C.size_t := C.size_t (Size);
      Mapped_Address : C.void_ptr;
   begin
      Mapped_Address := C.sys.mman.mmap (
         C.void_ptr (Null_Address),
         Mapped_Size,
         Protects (Writable),
         C.sys.mman.MAP_FILE + C.sys.mman.MAP_SHARED,
         Handle,
         Mapped_Offset);
      if Address (Mapped_Address) = Address (C.sys.mman.MAP_FAILED) then
         Raise_Exception (Use_Error'Identity);
      end if;
      Mapping.Storage_Address := Address (Mapped_Address);
      Mapping.Storage_Size := Storage_Elements.Storage_Offset (Size);
   end Map;

   procedure Unmap (
      Mapping : in out Mapping_Type;
      Raise_On_Error : Boolean) is
   begin
      if C.sys.mman.munmap (
         C.void_ptr (Mapping.Storage_Address),
         C.size_t (Mapping.Storage_Size)) /= 0
      then
         if Raise_On_Error then
            Raise_Exception (Use_Error'Identity);
         end if;
      end if;
      Mapping.Storage_Address := Null_Address;
      Mapping.Storage_Size := 0;
   end Unmap;

   function IO_Exception_Id (errno : C.signed_int)
      return Ada.Exception_Identification.Exception_Id is
   begin
      case errno is
         when C.errno.EIO =>
            return Device_Error'Identity;
         when others =>
            return Use_Error'Identity;
      end case;
   end IO_Exception_Id;

end System.Native_IO;
