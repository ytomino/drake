with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
with C.errno;
with C.stdlib;
with C.string;
with C.sys.fcntl;
with C.sys.stat;
with C.sys.types;
with C.sys.unistd;
with C.unistd;
package body Ada.Streams.Stream_IO.Inside is
   use type Ada.Tags.Tag;
   use type System.Storage_Elements.Storage_Offset;
   use type C.char;
   use type C.char_array;
   use type C.char_ptr;
   use type C.signed_int; -- ssize_t is signed int or signed long
   use type C.signed_long;
   use type C.size_t;
   use type C.unsigned_short;
   use type C.unsigned_int;
   use type C.sys.types.off_t;

   procedure Free is new Unchecked_Deallocation (
      Stream_Type,
      Non_Controlled_File_Type);

   function realloc_strcat (
      s : C.char_ptr;
      a : not null access constant C.char) return C.char_ptr;
   function realloc_strcat (
      s : C.char_ptr;
      a : String) return C.char_ptr;

   type Open_Method is (Open, Create, Reset);
   pragma Discard_Names (Open_Method);

   function Form_Share_Mode (Form : String; Default : C.unsigned_int)
      return C.unsigned_int;

   procedure Check_File_Open (File : Non_Controlled_File_Type);

   function Index_Impl (Stream : Stream_Type)
      return Stream_Element_Positive_Count;
   procedure Read_Impl (
      Stream : in out Stream_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);
   procedure Set_Index_Impl (
      Stream : in out Stream_Type;
      To : Stream_Element_Positive_Count);
   function Size_Impl (Stream : Stream_Type)
      return Stream_Element_Count;
   procedure Write_Impl (
      Stream : in out Stream_Type;
      Item : Stream_Element_Array);

   procedure Open_File (
      Method : Open_Method;
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String);

   procedure Set_Index_To_End (File : Non_Controlled_File_Type);

   procedure Check_File_Open (File : Non_Controlled_File_Type) is
   begin
      if File = null then
         raise Status_Error;
      end if;
   end Check_File_Open;

   --  implementation

   procedure Close (
      File : in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True) is
   begin
      Check_File_Open (File);
      case File.Kind is
         when Normal | Temporary | External =>
            declare
               Handle : constant Handle_Type := File.Handle;
               Z_Name : String := File.Name & Character'Val (0);
               Kind : constant Stream_Kind := File.Kind;
               Error : Boolean;
            begin
               Free (File);
               Error := C.unistd.close (Handle) < 0;
               if Kind = Temporary then
                  declare
                     C_Name : C.char_array (0 .. Z_Name'Length - 1);
                     for C_Name'Address use Z_Name'Address;
                  begin
                     if C.unistd.unlink (C_Name (C_Name'First)'Access) < 0 then
                        Error := True;
                     end if;
                  end;
               end if;
               if Error and then Raise_On_Error then
                  raise Use_Error;
               end if;
            end;
         when External_No_Close =>
            Free (File);
         when Standard_Handle =>
            null; -- statically allocated
      end case;
   end Close;

   procedure Create (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "") is
   begin
      if File /= null then
         raise Status_Error;
      end if;
      Open_File (
         Method => Create,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
      if Mode = Append_File then
         Set_Index_To_End (File); -- Append_File sets index to the last
      end if;
   end Create;

   procedure Delete (File : in out Non_Controlled_File_Type) is
   begin
      Check_File_Open (File);
      case File.Kind is
         when Normal | Temporary =>
            File.Kind := Temporary;
            Close (File, Raise_On_Error => True);
         when External | External_No_Close | Standard_Handle =>
            raise Status_Error;
      end case;
   end Delete;

   function End_Of_File (File : Non_Controlled_File_Type) return Boolean is
      Info : aliased C.sys.stat.struct_stat;
   begin
      Check_File_Open (File);
      if C.sys.stat.fstat (File.Handle, Info'Access) < 0 then
         raise Device_Error;
      end if;
      if (Info.st_mode and C.sys.stat.S_IFMT) = C.sys.stat.S_IFIFO then
         if File.Last > 0 then
            return False;
         end if;
         declare
            Read_Size : C.sys.types.ssize_t;
         begin
            Read_Size := C.unistd.read (
               File.Handle,
               C.void_ptr (File.Buffer (1)'Address),
               1);
            if Read_Size < 0 then
               raise Use_Error;
            end if;
            if Read_Size = 0 then
               return True;
            end if;
            File.Last := 1;
            return False;
         end;
      else
         declare
            Index : constant C.sys.types.off_t :=
               C.unistd.lseek (File.Handle, 0, C.sys.unistd.SEEK_CUR);
         begin
            if Index < 0 then
               raise Use_Error;
            end if;
            return Index >= Info.st_size;
         end;
      end if;
   end End_Of_File;

   procedure Flush (File : Non_Controlled_File_Type) is
   begin
      Check_File_Open (File);
      if C.unistd.fsync (File.Handle) < 0 then
         --  EINVAL means fd is not file but FIFO, etc.
         if C.errno.errno /= C.errno.EINVAL then
            raise Device_Error;
         end if;
      end if;
   end Flush;

   function Form (File : Non_Controlled_File_Type) return String is
   begin
      Check_File_Open (File);
      return File.Form;
   end Form;

   procedure Form_Parameter (
      Form : String;
      Keyword : String;
      First : out Positive;
      Last : out Natural) is
   begin
      for J in Form'First + Keyword'Length .. Form'Last - 1 loop
         if Form (J) = '='
           and then Form (J - Keyword'Length .. J - 1) = Keyword
         then
            First := J + 1;
            Last := First - 1;
            while Last < Form'Last and then Form (Last + 1) /= ',' loop
               Last := Last + 1;
            end loop;
            return;
         end if;
      end loop;
      First := Form'First;
      Last := First - 1;
   end Form_Parameter;

   function Form_Share_Mode (Form : String; Default : C.unsigned_int)
      return C.unsigned_int
   is
      First : Positive;
      Last : Natural;
   begin
      Form_Parameter (Form, "shared", First, Last);
      if First <= Last and then Form (First) = 'y' then
         return C.sys.fcntl.O_SHLOCK;
      elsif First <= Last and then Form (First) = 'n' then
         return C.sys.fcntl.O_EXLOCK;
      else
         return Default;
      end if;
   end Form_Share_Mode;

   function Handle (File : File_Type) return Handle_Type is
   begin
      return Handle (Reference (File).all);
   end Handle;

   function Handle (File : Non_Controlled_File_Type) return Handle_Type is
   begin
      Check_File_Open (File);
      return File.Handle;
   end Handle;

   function Index (File : Non_Controlled_File_Type) return Positive_Count is
   begin
      Check_File_Open (File);
      return Index_Impl (File.all);
   end Index;

   function Index_Impl (Stream : Stream_Type)
      return Stream_Element_Positive_Count
   is
      Result : C.sys.types.off_t;
   begin
      Result := C.unistd.lseek (Stream.Handle, 0, C.sys.unistd.SEEK_CUR);
      if Result < 0 then
         raise Use_Error;
      end if;
      return Positive_Count (Result + 1);
   end Index_Impl;

   function Is_Open (File : Non_Controlled_File_Type) return Boolean is
   begin
      return File /= null;
   end Is_Open;

   function Is_Terminal (File : Non_Controlled_File_Type) return Boolean is
   begin
      Check_File_Open (File);
      return C.unistd.isatty (File.Handle) /= 0;
   end Is_Terminal;

   function Mode (File : Non_Controlled_File_Type) return File_Mode is
   begin
      Check_File_Open (File);
      return File.Mode;
   end Mode;

   function Name (File : Non_Controlled_File_Type) return String is
   begin
      Check_File_Open (File);
      return File.Name;
   end Name;

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
         Name,
         Form,
         To_Close);
   end Open;

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "") is
   begin
      if File /= null then
         raise Status_Error;
      end if;
      Open_File (
         Method => Open,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
      if Mode = Append_File then
         Set_Index_To_End (File); -- Append_File sets index to the last
      end if;
   end Open;

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Handle : Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "";
      To_Close : Boolean := False)
   is
      Kind : Stream_Kind;
   begin
      if File /= null then
         raise Status_Error;
      end if;
      if To_Close then
         Kind := External;
      else
         Kind := External_No_Close;
      end if;
      File := new Stream_Type'(
         Name_Length => Name'Length + 1,
         Form_Length => Form'Length,
         Dispatcher => (others => <>),
         Handle => Handle,
         Mode => Mode,
         Kind => Kind,
         Buffer => <>,
         Last => 0,
         Name => '*' & Name,
         Form => Form);
   end Open;

   procedure Open_File (
      Method : Open_Method;
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String)
   is
      Temp_Var : constant C.char_array := "TMPDIR" & C.char'Val (0);
      Temp_Template : constant String := "ADAXXXXXX";
      Temp_Dir : C.char_ptr;
      Flags : C.unsigned_int;
      Default_Lock_Flags : C.unsigned_int;
      Modes : constant := 8#644#;
      Kind : Stream_Kind;
      Full_Name : C.char_ptr;
      Handle : Handle_Type;
      errno : C.signed_int;
   begin
      if Name /= "" then
         --  absolute path
         if Name (Name'First) = '/' then
            Full_Name := realloc_strcat (null, Name);
         else
            --  current directory
            Full_Name := C.unistd.getcwd (null, 0);
            --  add slash
            declare
               subtype Fixed_char_array is C.char_array (C.size_t);
               Name : Fixed_char_array;
               for Name'Address use Full_Name.all'Address;
            begin
               if Name (C.string.strlen (Full_Name) - 1) /= '/' then
                  Full_Name := realloc_strcat (Full_Name, "/");
               end if;
            end;
            --  add name
            Full_Name := realloc_strcat (Full_Name, Name);
         end if;
         --  Normal File
         Kind := Normal;
         --  Flags, Append_File always has read and write access for Inout_File
         if Mode = In_File then
            Default_Lock_Flags := C.sys.fcntl.O_SHLOCK;
         else
            Default_Lock_Flags := C.sys.fcntl.O_EXLOCK;
         end if;
         case Method is
            when Create =>
               declare
                  use C.sys.fcntl;
                  Table : constant array (File_Mode) of C.unsigned_int := (
                     In_File => O_RDWR or O_CREAT or O_TRUNC,
                     Out_File => O_WRONLY or O_CREAT or O_TRUNC,
                     Append_File => O_RDWR or O_CREAT); -- no truncation
               begin
                  Flags := Table (Mode);
                  Default_Lock_Flags := O_EXLOCK;
               end;
            when Open =>
               declare
                  use C.sys.fcntl;
                  Table : constant array (File_Mode) of C.unsigned_int := (
                     In_File => O_RDONLY,
                     Out_File => O_WRONLY or O_TRUNC,
                     Append_File => O_RDWR); -- O_APPEND ignores lseek
               begin
                  Flags := Table (Mode);
               end;
            when Reset =>
               declare
                  use C.sys.fcntl;
                  Table : constant array (File_Mode) of C.unsigned_int := (
                     In_File => O_RDONLY,
                     Out_File => O_WRONLY,
                     Append_File => O_RDWR); -- O_APPEND ignores lseek
               begin
                  Flags := Table (Mode);
               end;
         end case;
         Flags := Flags or Form_Share_Mode (Form, Default_Lock_Flags);
         --  Open
         Handle := C.sys.fcntl.open (
            Full_Name,
            C.signed_int (Flags),
            Modes);
         if Handle < 0 then
            errno := C.errno.errno;
            C.stdlib.free (C.void_ptr (Full_Name.all'Address));
            case errno is
               when C.errno.ENOTDIR
                  | C.errno.ENAMETOOLONG
                  | C.errno.ENOENT
                  | C.errno.EACCES
                  | C.errno.EISDIR
                  | C.errno.EROFS =>
                  raise Name_Error;
               when others =>
                  raise Use_Error;
            end case;
         else
            declare
               Dummy : C.signed_int;
               pragma Unreferenced (Dummy);
            begin
               Dummy := C.sys.fcntl.fcntl (
                  Handle,
                  C.sys.fcntl.F_SETFD,
                  C.sys.fcntl.FD_CLOEXEC);
            end;
         end if;
      else
         Temp_Dir := C.stdlib.getenv (Temp_Var (0)'Access);
         if Temp_Dir /= null then
            Full_Name := realloc_strcat (null, Temp_Dir);
         else
            Full_Name := C.unistd.getcwd (null, 0);
         end if;
         --  add slash
         declare
            subtype Fixed_char_array is C.char_array (C.size_t);
            Name : Fixed_char_array;
            for Name'Address use Full_Name.all'Address;
         begin
            if Name (C.string.strlen (Full_Name) - 1) /= '/' then
               Full_Name := realloc_strcat (Full_Name, "/");
            end if;
         end;
         --  add template
         Full_Name := realloc_strcat (Full_Name, Temp_Template);
         --  Temporary File
         Kind := Temporary;
         --  Open
         Handle := C.unistd.mkstemp (Full_Name);
         if Handle < 0 then
            C.stdlib.free (C.void_ptr (Full_Name.all'Address));
            raise Use_Error;
         end if;
      end if;
      declare
         subtype Fixed_String is String (Positive);
         Name : Fixed_String;
         for Name'Address use Full_Name.all'Address;
         Name_Length : constant Natural :=
            Integer (C.string.strlen (Full_Name));
      begin
         File := new Stream_Type'(
            Name_Length => Name_Length,
            Form_Length => Form'Length,
            Dispatcher => (others => <>),
            Handle => Handle,
            Mode => Mode,
            Kind => Kind,
            Buffer => <>,
            Last => 0,
            Name => Name (1 .. Name_Length),
            Form => Form);
         C.stdlib.free (C.void_ptr (Full_Name.all'Address));
      end;
   end Open_File;

   procedure Read (
      File : Non_Controlled_File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset) is
   begin
      Check_File_Open (File);
      Read_Impl (File.all, Item, Last);
   end Read;

   procedure Read_Impl (
      Stream : in out Stream_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Read_Size : C.sys.types.ssize_t;
      First : Stream_Element_Offset := Item'First;
   begin
      if Stream.Mode = Out_File then
         raise Mode_Error;
      end if;
      if First > Item'Last then
         Last := First - 1;
         return;
      end if;
      if Stream.Last > 0 then
         Item (First) := Stream.Buffer (1);
         First := First + 1;
         Stream.Last := 0;
         if First > Item'Last then
            Last := Item'First;
            return;
         end if;
      end if;
      Read_Size := C.unistd.read (
         Stream.Handle,
         C.void_ptr (Item (First)'Address),
         C.size_t (Item'Last - First + 1));
      if Read_Size >= 0 then
         Last := First + Stream_Element_Offset (Read_Size) - 1;
      else
         raise End_Error;
      end if;
   end Read_Impl;

   function realloc_strcat (
      s : C.char_ptr;
      a : not null access constant C.char) return C.char_ptr
   is
      Right_Length : constant C.size_t := C.string.strlen (a);
      Right : String (1 .. Natural (Right_Length));
      for Right'Address use a.all'Address;
   begin
      return realloc_strcat (s, Right);
   end realloc_strcat;

   function realloc_strcat (
      s : C.char_ptr;
      a : String) return C.char_ptr
   is
      function Cast is new Unchecked_Conversion (C.char_ptr, C.void_ptr);
      function Cast is new Unchecked_Conversion (C.void_ptr, C.char_ptr);
      Left_Length : C.size_t;
      Right_Length : C.size_t;
   begin
      if s /= null then
         Left_Length := C.string.strlen (s);
      else
         Left_Length := 0;
      end if;
      Right_Length := a'Length;
      return Result : constant C.char_ptr :=
         Cast (C.stdlib.realloc (Cast (s), Left_Length + Right_Length + 1))
      do
         if Result = null then
            C.stdlib.free (Cast (s));
            raise Storage_Error;
         else
            declare
               Right : C.void_ptr := C.void_ptr (
                  System.Address (Cast (Result)) +
                  System.Storage_Elements.Storage_Offset (Left_Length));
               Dummy : C.void_ptr;
               pragma Unreferenced (Dummy);
            begin
               Dummy := C.string.memcpy (
                  Right,
                  C.void_const_ptr (a'Address),
                  Right_Length);
               Right := C.void_ptr (
                  System.Address (Right) +
                  System.Storage_Elements.Storage_Offset (Right_Length));
               Cast (Right).all := C.char'Val (0);
            end;
         end if;
      end return;
   end realloc_strcat;

   procedure Reset (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode) is
   begin
      Check_File_Open (File);
      case File.Kind is
         when Normal =>
            declare
               Name : constant String := File.Name;
               Form : constant String := File.Form;
            begin
               Close (File, Raise_On_Error => True);
               Open_File (
                  Method => Reset,
                  File => File,
                  Mode => Mode,
                  Name => Name,
                  Form => Form);
            end;
         when Temporary =>
            File.Mode := Mode;
            Set_Index (File, 1);
         when External | External_No_Close | Standard_Handle =>
            raise Status_Error;
      end case;
      if Mode = Append_File then
         Set_Index_To_End (File);
      end if;
   end Reset;

   procedure Set_Index (
      File : Non_Controlled_File_Type;
      To : Positive_Count) is
   begin
      Check_File_Open (File);
      Set_Index_Impl (File.all, To);
   end Set_Index;

   procedure Set_Index_Impl (
      Stream : in out Stream_Type;
      To : Stream_Element_Positive_Count) is
   begin
      if C.unistd.lseek (
         Stream.Handle,
         C.sys.types.off_t (To) - 1,
         C.sys.unistd.SEEK_SET) < 0
      then
         raise Use_Error;
      end if;
   end Set_Index_Impl;

   procedure Set_Index_To_End (File : Non_Controlled_File_Type) is
   begin
      if C.unistd.lseek (File.Handle, 0, C.sys.unistd.SEEK_END) < 0 then
         raise Use_Error;
      end if;
   end Set_Index_To_End;

   procedure Set_Mode (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode)
   is
      Current : Positive_Count;
   begin
      Check_File_Open (File);
      Current := Index (File);
      case File.Kind is
         when Normal =>
            declare
               Name : constant String := File.Name;
               Form : constant String := File.Form;
            begin
               Close (File, Raise_On_Error => True);
               Open_File (
                  Method => Reset,
                  File => File,
                  Mode => Mode,
                  Name => Name,
                  Form => Form);
            end;
         when Temporary =>
            File.Mode := Mode;
         when External | External_No_Close | Standard_Handle =>
            raise Status_Error;
      end case;
      if Mode = Append_File then
         Set_Index_To_End (File);
      else
         Set_Index (File, Current);
      end if;
   end Set_Mode;

   function Size (File : Non_Controlled_File_Type) return Count is
   begin
      Check_File_Open (File);
      return Size_Impl (File.all);
   end Size;

   function Size_Impl (Stream : Stream_Type)
      return Stream_Element_Count
   is
      Info : aliased C.sys.stat.struct_stat;
   begin
      if C.sys.stat.fstat (Stream.Handle, Info'Access) < 0 then
         raise Device_Error;
      end if;
      return Count (Info.st_size);
   end Size_Impl;

   function Stream (File : Non_Controlled_File_Type) return Stream_Access is
      package Conv is new System.Address_To_Named_Access_Conversions (
         Root_Stream_Type'Class,
         Stream_Access);
   begin
      Check_File_Open (File);
      if File.Dispatcher.Tag = Ada.Tags.No_Tag then
         if C.unistd.lseek (File.Handle, 0, C.sys.unistd.SEEK_CUR) < 0 then
            File.Dispatcher.Tag := Dispatchers.Root_Dispatcher'Tag;
         else
            File.Dispatcher.Tag := Dispatchers.Seekable_Dispatcher'Tag;
         end if;
         File.Dispatcher.Stream := File;
      end if;
      return Conv.To_Pointer (File.Dispatcher'Address);
   end Stream;

   procedure Write (
      File : Non_Controlled_File_Type;
      Item : Stream_Element_Array) is
   begin
      Check_File_Open (File);
      Write_Impl (File.all, Item);
   end Write;

   procedure Write_Impl (
      Stream : in out Stream_Type;
      Item : Stream_Element_Array) is
   begin
      if Stream.Mode = In_File then
         raise Mode_Error;
      end if;
      if C.unistd.write (
         Stream.Handle,
         C.void_const_ptr (Item'Address),
         Item'Length) < 0
      then
         case C.errno.errno is
            when C.errno.EPIPE =>
               null;
            when others =>
               raise Use_Error;
         end case;
      end if;
   end Write_Impl;

   package body Dispatchers is

      overriding procedure Read (
         Stream : in out Root_Dispatcher;
         Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset) is
      begin
         Read_Impl (Stream.Stream.all, Item, Last);
      end Read;

      overriding procedure Write (
         Stream : in out Root_Dispatcher;
         Item : Stream_Element_Array) is
      begin
         Write_Impl (Stream.Stream.all, Item);
      end Write;

      overriding procedure Read (
         Stream : in out Seekable_Dispatcher;
         Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset) is
      begin
         Read_Impl (Stream.Stream.all, Item, Last);
      end Read;

      overriding procedure Write (
         Stream : in out Seekable_Dispatcher;
         Item : Stream_Element_Array) is
      begin
         Write_Impl (Stream.Stream.all, Item);
      end Write;

      overriding procedure Set_Index (
         Stream : in out Seekable_Dispatcher;
         To : Stream_Element_Positive_Count) is
      begin
         Set_Index_Impl (Stream.Stream.all, To);
      end Set_Index;

      overriding function Index (Stream : Seekable_Dispatcher)
         return Stream_Element_Positive_Count is
      begin
         return Index_Impl (Stream.Stream.all);
      end Index;

      overriding function Size (Stream : Seekable_Dispatcher)
         return Stream_Element_Count is
      begin
         return Size_Impl (Stream.Stream.all);
      end Size;

   end Dispatchers;

end Ada.Streams.Stream_IO.Inside;
