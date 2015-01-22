with Ada.Exceptions.Finally;
with Ada.Directories.Inside;
with Ada.Exception_Identification.From_Here;
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Native_Credentials;
with System.Native_Time;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.errno;
with C.sys.stat;
with C.sys.types;
with C.unistd;
package body Ada.Directories.Information is
   use Exception_Identification.From_Here;
   use type C.size_t;
   use type C.sys.types.mode_t;
   use type C.sys.types.ssize_t;

   procedure Fill (
      Directory_Entry : not null access Non_Controlled_Directory_Entry_Type);
   procedure Fill (
      Directory_Entry : not null access Non_Controlled_Directory_Entry_Type) is
   begin
      if Directory_Entry.Status = Empty then
         Raise_Exception (Status_Error'Identity);
      end if;
      if not Directory_Entry.Additional.Filled then
         Directory_Searching.Get_Information (
            Directory_Entry.Path.all,
            Directory_Entry.Directory_Entry,
            Directory_Entry.Additional.Information);
         Directory_Entry.Additional.Filled := True;
      end if;
   end Fill;

   function To_Permission_Set (Mode : C.sys.types.mode_t)
      return Permission_Set_Type;
   function To_Permission_Set (Mode : C.sys.types.mode_t)
      return Permission_Set_Type is
   begin
      return (
         Others_Execute => (Mode and C.sys.stat.S_IXOTH) /= 0,
         Others_Write => (Mode and C.sys.stat.S_IWOTH) /= 0,
         Others_Read => (Mode and C.sys.stat.S_IROTH) /= 0,
         Group_Execute => (Mode and C.sys.stat.S_IXGRP) /= 0,
         Group_Write => (Mode and C.sys.stat.S_IWGRP) /= 0,
         Group_Read => (Mode and C.sys.stat.S_IRGRP) /= 0,
         Owner_Execute => (Mode and C.sys.stat.S_IXUSR) /= 0,
         Owner_Write => (Mode and C.sys.stat.S_IWUSR) /= 0,
         Owner_Read => (Mode and C.sys.stat.S_IRUSR) /= 0,
         Set_Group_ID => (Mode and C.sys.stat.S_ISGID) /= 0,
         Set_User_ID => (Mode and C.sys.stat.S_ISUID) /= 0);
   end To_Permission_Set;

   --  implementation

   function Group (Name : String) return String is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return System.Native_Credentials.Group_Name (Information.st_gid);
   end Group;

   function Group (Directory_Entry : Directory_Entry_Type) return String is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return System.Native_Credentials.Group_Name (
         NC_Directory_Entry.Additional.Information.st_gid);
   end Group;

   function Is_Block_Special_File (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFBLK;
   end Is_Block_Special_File;

   function Is_Block_Special_File (Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
         and C.sys.stat.S_IFMT) = C.sys.stat.S_IFBLK;
   end Is_Block_Special_File;

   function Is_Character_Special_File (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFCHR;
   end Is_Character_Special_File;

   function Is_Character_Special_File (Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
         and C.sys.stat.S_IFMT) = C.sys.stat.S_IFCHR;
   end Is_Character_Special_File;

   function Is_FIFO (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFIFO;
   end Is_FIFO;

   function Is_FIFO (Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
         and C.sys.stat.S_IFMT) = C.sys.stat.S_IFIFO;
   end Is_FIFO;

   function Is_Socket (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFSOCK;
   end Is_Socket;

   function Is_Socket (Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
         and C.sys.stat.S_IFMT) = C.sys.stat.S_IFSOCK;
   end Is_Socket;

   function Is_Symbolic_Link (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFLNK;
   end Is_Symbolic_Link;

   function Is_Symbolic_Link (Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
         and C.sys.stat.S_IFMT) = C.sys.stat.S_IFLNK;
   end Is_Symbolic_Link;

   function Last_Access_Time (Name : String) return Calendar.Time is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return Cast (System.Native_Time.To_Time (Information.st_atim));
   end Last_Access_Time;

   function Last_Access_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return Cast (System.Native_Time.To_Time (
         NC_Directory_Entry.Additional.Information.st_atim));
   end Last_Access_Time;

   function Last_Status_Change_Time (Name : String)
      return Calendar.Time
   is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return Cast (System.Native_Time.To_Time (Information.st_ctim));
   end Last_Status_Change_Time;

   function Last_Status_Change_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return Cast (System.Native_Time.To_Time (
         NC_Directory_Entry.Additional.Information.st_ctim));
   end Last_Status_Change_Time;

   function Owner (Name : String) return String is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return System.Native_Credentials.User_Name (Information.st_uid);
   end Owner;

   function Owner (Directory_Entry : Directory_Entry_Type) return String is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return System.Native_Credentials.User_Name (
         NC_Directory_Entry.Additional.Information.st_uid);
   end Owner;

   function Permission_Set (Name : String) return Permission_Set_Type is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return To_Permission_Set (Information.st_mode);
   end Permission_Set;

   function Permission_Set (Directory_Entry : Directory_Entry_Type)
      return Permission_Set_Type
   is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      Fill (NC_Directory_Entry);
      return To_Permission_Set (
         NC_Directory_Entry.Additional.Information.st_mode);
   end Permission_Set;

   function Read_Symbolic_Link (Name : String) return String is
      package Conv is
         new System.Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      procedure Finally (X : not null access C.char_ptr);
      procedure Finally (X : not null access C.char_ptr) is
      begin
         System.Standard_Allocators.Free (Conv.To_Address (X.all));
      end Finally;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (C.char_ptr, Finally);
      C_Name : C.char_array (
         0 ..
         Name'Length * System.Zero_Terminated_Strings.Expanding);
      Buffer_Length : C.size_t := 1024;
      Buffer : aliased C.char_ptr :=
         Conv.To_Pointer (System.Standard_Allocators.Allocate (
            System.Storage_Elements.Storage_Count (Buffer_Length)));
   begin
      Holder.Assign (Buffer'Access);
      System.Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      loop
         declare
            function To_size (X : C.size_t) return C.size_t renames "+"; -- OSX
            function To_size (X : C.size_t) return C.signed_int; -- FreeBSD
            function To_size (X : C.size_t) return C.signed_int is
            begin
               return C.signed_int (X);
            end To_size;
            pragma Warnings (Off, To_size);
            Result : constant C.sys.types.ssize_t := C.unistd.readlink (
               C_Name (0)'Access,
               Buffer,
               To_size (Buffer_Length));
         begin
            if Result < 0 then
               case C.errno.errno is
                  when C.errno.ENAMETOOLONG
                     | C.errno.ENOENT
                     | C.errno.ENOTDIR
                  =>
                     Raise_Exception (Name_Error'Identity);
                  when others =>
                     Raise_Exception (Use_Error'Identity);
               end case;
            end if;
            if C.size_t (Result) < Buffer_Length then
               return System.Zero_Terminated_Strings.Value (
                  Buffer,
                  C.size_t (Result));
            end if;
            Buffer_Length := Buffer_Length * 2;
            Buffer := Conv.To_Pointer (
               System.Standard_Allocators.Reallocate (
                  Conv.To_Address (Buffer),
                  System.Storage_Elements.Storage_Count (Buffer_Length)));
         end;
      end loop;
   end Read_Symbolic_Link;

   function Read_Symbolic_Link (Directory_Entry : Directory_Entry_Type)
      return String is
   begin
      return Read_Symbolic_Link (Full_Name (Directory_Entry));
   end Read_Symbolic_Link;

   function Identity (Name : String) return File_Id is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information);
      return File_Id (Information.st_ino);
   end Identity;

   function Identity (Directory_Entry : Directory_Entry_Type) return File_Id is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      if NC_Directory_Entry.Status = Empty then
         Raise_Exception (Status_Error'Identity);
      end if;
      return File_Id (NC_Directory_Entry.Directory_Entry.d_ino);
   end Identity;

end Ada.Directories.Information;
