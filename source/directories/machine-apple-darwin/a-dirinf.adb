with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Native_Calendar;
with System.Native_Credentials;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.errno;
with C.sys.stat;
with C.sys.types;
with C.unistd;
package body Ada.Directories.Information is
   use Exception_Identification.From_Here;
   use type System.Bit_Order;
   use type C.size_t;
   use type C.sys.types.mode_t;
   use type C.sys.types.ssize_t;

   subtype Directory_Entry_Information_Type is
      System.Native_Directories.Directory_Entry_Information_Type;

   function Named_IO_Exception_Id (errno : C.signed_int)
      return Exception_Identification.Exception_Id
      renames System.Native_Directories.Named_IO_Exception_Id;

   package char_ptr_Conv is
      new System.Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   procedure Fill (
      Directory_Entry : aliased in out Non_Controlled_Directory_Entry_Type);
   procedure Fill (
      Directory_Entry : aliased in out Non_Controlled_Directory_Entry_Type) is
   begin
      if not Directory_Entry.Additional.Filled then
         System.Native_Directories.Searching.Get_Information (
            Directory_Entry.Path.all,
            Directory_Entry.Directory_Entry,
            Directory_Entry.Additional.Information);
         Directory_Entry.Additional.Filled := True;
      end if;
   end Fill;

   function To_Permission_Set (Mode : C.sys.types.mode_t)
      return Permission_Set_Type;
   function To_Permission_Set (Mode : C.sys.types.mode_t)
      return Permission_Set_Type
   is
      Castable : constant Boolean :=
         System.Default_Bit_Order = System.Low_Order_First
         and then C.sys.stat.S_IXOTH = 8#001#
         and then C.sys.stat.S_IWOTH = 8#002#
         and then C.sys.stat.S_IROTH = 8#004#
         and then C.sys.stat.S_IXGRP = 8#010#
         and then C.sys.stat.S_IWGRP = 8#020#
         and then C.sys.stat.S_IRGRP = 8#040#
         and then C.sys.stat.S_IXUSR = 8#100#
         and then C.sys.stat.S_IWUSR = 8#200#
         and then C.sys.stat.S_IRUSR = 8#400#
         and then C.sys.stat.S_ISVTX = 8#1000#
         and then C.sys.stat.S_ISGID = 8#2000#
         and then C.sys.stat.S_ISUID = 8#4000#;
   begin
      if Castable then
         declare
            type Unsigned_12 is mod 2 ** 12;
            function Cast is
               new Unchecked_Conversion (Unsigned_12, Permission_Set_Type);
         begin
            return Cast (Unsigned_12'Mod (Mode and 8#7777#));
         end;
      else
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
            Sticky => (Mode and C.sys.stat.S_ISVTX) /= 0,
            Set_Group_ID => (Mode and C.sys.stat.S_ISGID) /= 0,
            Set_User_ID => (Mode and C.sys.stat.S_ISUID) /= 0);
      end if;
   end To_Permission_Set;

   function To_User_Permission_Set (Information : C.sys.stat.struct_stat)
      return User_Permission_Set_Type;
   function To_User_Permission_Set (Information : C.sys.stat.struct_stat)
      return User_Permission_Set_Type
   is
      Executable : Boolean;
      Writable : Boolean;
      Readable : Boolean;
   begin
      if System.Native_Credentials.Belongs_To_Current_User (
         Information.st_uid)
      then
         Executable := (Information.st_mode and C.sys.stat.S_IXUSR) /= 0;
         Writable := (Information.st_mode and C.sys.stat.S_IWUSR) /= 0;
         Readable := (Information.st_mode and C.sys.stat.S_IRUSR) /= 0;
      elsif System.Native_Credentials.Belongs_To_Current_Group (
         Information.st_gid)
      then
         Executable := (Information.st_mode and C.sys.stat.S_IXGRP) /= 0;
         Writable := (Information.st_mode and C.sys.stat.S_IWGRP) /= 0;
         Readable := (Information.st_mode and C.sys.stat.S_IRGRP) /= 0;
      else
         Executable := (Information.st_mode and C.sys.stat.S_IXOTH) /= 0;
         Writable := (Information.st_mode and C.sys.stat.S_IWOTH) /= 0;
         Readable := (Information.st_mode and C.sys.stat.S_IROTH) /= 0;
      end if;
      return (
         User_Execute => Executable,
         User_Write => Writable,
         User_Read => Readable);
   end To_User_Permission_Set;

   --  implementation

   function Last_Access_Time (Name : String) return Calendar.Time is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return Cast (System.Native_Calendar.To_Time (Information.st_atim));
   end Last_Access_Time;

   function Last_Status_Change_Time (Name : String) return Calendar.Time is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return Cast (System.Native_Calendar.To_Time (Information.st_ctim));
   end Last_Status_Change_Time;

   function Permission_Set (Name : String) return Permission_Set_Type is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return To_Permission_Set (Information.st_mode);
   end Permission_Set;

   function Owner (Name : String) return String is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return System.Native_Credentials.User_Name (Information.st_uid);
   end Owner;

   function Group (Name : String) return String is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return System.Native_Credentials.Group_Name (Information.st_gid);
   end Group;

   function Is_Block_Special_File (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) = C.sys.stat.S_IFBLK;
   end Is_Block_Special_File;

   function Is_Character_Special_File (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) = C.sys.stat.S_IFCHR;
   end Is_Character_Special_File;

   function Is_FIFO (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) = C.sys.stat.S_IFIFO;
   end Is_FIFO;

   function Is_Symbolic_Link (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) = C.sys.stat.S_IFLNK;
   end Is_Symbolic_Link;

   function Is_Socket (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.st_mode and C.sys.stat.S_IFMT) = C.sys.stat.S_IFSOCK;
   end Is_Socket;

   function Last_Access_Time (
      Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return Cast (
         System.Native_Calendar.To_Time (
            NC_Directory_Entry.Additional.Information.st_atim));
   end Last_Access_Time;

   function Last_Status_Change_Time (
      Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return Cast (
         System.Native_Calendar.To_Time (
            NC_Directory_Entry.Additional.Information.st_ctim));
   end Last_Status_Change_Time;

   function Permission_Set (
      Directory_Entry : Directory_Entry_Type)
      return Permission_Set_Type
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return To_Permission_Set (
         NC_Directory_Entry.Additional.Information.st_mode);
   end Permission_Set;

   function Owner (
      Directory_Entry : Directory_Entry_Type)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return System.Native_Credentials.User_Name (
         NC_Directory_Entry.Additional.Information.st_uid);
   end Owner;

   function Group (
      Directory_Entry : Directory_Entry_Type)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return System.Native_Credentials.Group_Name (
         NC_Directory_Entry.Additional.Information.st_gid);
   end Group;

   function Is_Block_Special_File (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
            and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFBLK;
   end Is_Block_Special_File;

   function Is_Character_Special_File (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
            and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFCHR;
   end Is_Character_Special_File;

   function Is_FIFO (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
            and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFIFO;
   end Is_FIFO;

   function Is_Symbolic_Link (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
            and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFLNK;
   end Is_Symbolic_Link;

   function Is_Socket (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return (NC_Directory_Entry.Additional.Information.st_mode
            and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFSOCK;
   end Is_Socket;

   function Read_Symbolic_Link (Name : String) return String is
      procedure Finally (X : in out C.char_ptr);
      procedure Finally (X : in out C.char_ptr) is
      begin
         System.Standard_Allocators.Free (char_ptr_Conv.To_Address (X));
      end Finally;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (C.char_ptr, Finally);
      C_Name : C.char_array (
         0 ..
         Name'Length * System.Zero_Terminated_Strings.Expanding);
      Buffer_Length : C.size_t := 1024;
      Buffer : aliased C.char_ptr :=
         char_ptr_Conv.To_Pointer (
            System.Standard_Allocators.Allocate (
               System.Storage_Elements.Storage_Offset (Buffer_Length)));
   begin
      Holder.Assign (Buffer);
      System.Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      loop
         declare
            Length : constant C.sys.types.ssize_t :=
               C.unistd.readlink (C_Name (0)'Access, Buffer, Buffer_Length);
         begin
            if Length < 0 then
               Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
            end if;
            if C.size_t (Length) < Buffer_Length then
               return System.Zero_Terminated_Strings.Value (
                  Buffer,
                  C.size_t (Length));
            end if;
            Buffer_Length := Buffer_Length * 2;
            Buffer :=
               char_ptr_Conv.To_Pointer (
                  System.Standard_Allocators.Reallocate (
                     char_ptr_Conv.To_Address (Buffer),
                     System.Storage_Elements.Storage_Offset (Buffer_Length)));
         end;
      end loop;
   end Read_Symbolic_Link;

   function Read_Symbolic_Link (
      Directory_Entry : Directory_Entry_Type)
      return String is
   begin
      return Read_Symbolic_Link (
         Full_Name (Directory_Entry)); -- checking the predicate
   end Read_Symbolic_Link;

   function User_Permission_Set (Name : String)
      return User_Permission_Set_Type
   is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return To_User_Permission_Set (Information);
   end User_Permission_Set;

   function User_Permission_Set (
      Directory_Entry : Directory_Entry_Type)
      return User_Permission_Set_Type
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Fill (NC_Directory_Entry);
      return To_User_Permission_Set (
         NC_Directory_Entry.Additional.Information);
   end User_Permission_Set;

   function Identity (Name : String) return File_Id is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return File_Id (Information.st_ino);
   end Identity;

   function Identity (
      Directory_Entry : Directory_Entry_Type)
      return File_Id
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      return File_Id (NC_Directory_Entry.Directory_Entry.d_ino);
   end Identity;

end Ada.Directories.Information;
