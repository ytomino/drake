with Ada.Exception_Identification.From_Here;
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with C.errno;
with C.fcntl;
with C.stdlib;
with C.sys.file;
with C.sys.types;
with C.unistd;
package body System.Native_IO is
   use Ada.Exception_Identification.From_Here;
   use type Ada.IO_Modes.File_Shared;
   use type Ada.IO_Modes.File_Shared_Spec;
   use type C.char; -- Name_Character
   use type C.char_array; -- Name_String
   use type C.char_ptr; -- Name_Pointer
   use type C.size_t; -- Name_Length
   use type C.sys.types.off_t;
   use type C.unsigned_int;

   function strlen (Item : not null access constant C.char) return C.size_t;
   pragma Import (Intrinsic, strlen, "__builtin_strlen");

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
      function To_Address is
         new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      if Item (Item'First) = '/' then
         --  absolute path
         Out_Item := Name_Pointer_Conv.To_Pointer (
            To_Address (
               C.stdlib.malloc (
                  Item'Length * Zero_Terminated_Strings.Expanding
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
                  To_Address (
                     C.stdlib.realloc (
                        C.void_ptr (Name_Pointer_Conv.To_Address (Out_Item)),
                        Out_Length
                           + Item'Length * Zero_Terminated_Strings.Expanding
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
      Out_Length : out Name_Length)
   is
      function To_Address is
         new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      Out_Item := Name_Pointer_Conv.To_Pointer (
         To_Address (
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
      function To_Address is
         new Ada.Unchecked_Conversion (C.void_ptr, Address);
      Temp_Template_Length : constant C.size_t := Temp_Template'Length - 1;
      Temp_Dir : C.char_ptr;
   begin
      --  compose template
      Temp_Dir := C.stdlib.getenv (Temp_Variable (0)'Access);
      if Temp_Dir /= null then
         --  environment variable TMPDIR
         Out_Length := strlen (Temp_Dir);
         Out_Item := Name_Pointer_Conv.To_Pointer (
            To_Address (
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
                  To_Address (
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
         Raise_Exception (Use_Error'Identity);
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
               | C.errno.EACCES
               | C.errno.EEXIST -- O_EXCL
               | C.errno.EISDIR
               | C.errno.EROFS =>
               Raise_Exception (Name_Error'Identity);
            when others =>
               Raise_Exception (Use_Error'Identity);
         end case;
      end if;
      declare
         O_CLOEXEC_Is_Missing : constant Boolean :=
            C.fcntl.O_CLOEXEC = 0;
         pragma Warnings (Off, O_CLOEXEC_Is_Missing);
         Dummy : C.signed_int;
         pragma Unreferenced (Dummy);
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
         Dummy : C.signed_int;
         pragma Unreferenced (Dummy);
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

   procedure Close_Temporary (
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
         Raise_Exception (Use_Error'Identity);
      end if;
   end Close_Temporary;

   procedure Close_Ordinary (
      Handle : Handle_Type;
      Raise_On_Error : Boolean)
   is
      Error : Boolean;
   begin
      Error := C.unistd.close (Handle) < 0;
      if Error and then Raise_On_Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Close_Ordinary;

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

end System.Native_IO;
