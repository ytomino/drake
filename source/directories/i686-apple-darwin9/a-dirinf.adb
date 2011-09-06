with Ada.Permissions.Inside;
with Ada.Unchecked_Conversion;
with System.Native_Time;
with C.sys.stat;
with C.sys.types;
package body Ada.Directories.Information is
   use type C.sys.types.mode_t;

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
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return Permissions.Inside.Group_Name (Attributes.st_gid);
   end Group;

   function Group (Directory_Entry : Directory_Entry_Type) return String is
   begin
      Check_Assigned (Directory_Entry);
      return Permissions.Inside.Group_Name (Directory_Entry.State_Data.st_gid);
   end Group;

   function Is_Block_Special_File (Name : String) return Boolean is
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return (Attributes.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFBLK;
   end Is_Block_Special_File;

   function Is_Block_Special_File (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.State_Data.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFBLK;
   end Is_Block_Special_File;

   function Is_Character_Special_File (Name : String) return Boolean is
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return (Attributes.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFCHR;
   end Is_Character_Special_File;

   function Is_Character_Special_File (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.State_Data.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFCHR;
   end Is_Character_Special_File;

   function Is_FIFO (Name : String) return Boolean is
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return (Attributes.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFIFO;
   end Is_FIFO;

   function Is_FIFO (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.State_Data.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFIFO;
   end Is_FIFO;

   function Is_Socket (Name : String) return Boolean is
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return (Attributes.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFSOCK;
   end Is_Socket;

   function Is_Socket (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.State_Data.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFSOCK;
   end Is_Socket;

   function Is_Symbolic_Link (Name : String) return Boolean is
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return (Attributes.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFLNK;
   end Is_Symbolic_Link;

   function Is_Symbolic_Link (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.State_Data.st_mode and C.sys.stat.S_IFMT) =
         C.sys.stat.S_IFLNK;
   end Is_Symbolic_Link;

   function Last_Access_Time (Name : String) return Calendar.Time is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return Cast (System.Native_Time.To_Time (Attributes.st_atimespec));
   end Last_Access_Time;

   function Last_Access_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
   begin
      Check_Assigned (Directory_Entry);
      return Cast (
         System.Native_Time.To_Time (Directory_Entry.State_Data.st_atimespec));
   end Last_Access_Time;

   function Last_Status_Change_Time (Name : String)
      return Calendar.Time
   is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return Cast (System.Native_Time.To_Time (Attributes.st_ctimespec));
   end Last_Status_Change_Time;

   function Last_Status_Change_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
   begin
      Check_Assigned (Directory_Entry);
      return Cast (
         System.Native_Time.To_Time (Directory_Entry.State_Data.st_ctimespec));
   end Last_Status_Change_Time;

   function Owner (Name : String) return String is
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return Permissions.Inside.User_Name (Attributes.st_uid);
   end Owner;

   function Owner (Directory_Entry : Directory_Entry_Type) return String is
   begin
      Check_Assigned (Directory_Entry);
      return Permissions.Inside.User_Name (Directory_Entry.State_Data.st_uid);
   end Owner;

   function Permission_Set (Name : String) return Permission_Set_Type is
      Attributes : C.sys.stat.struct_stat;
   begin
      Get_Attributes (Name, Attributes);
      return To_Permission_Set (Attributes.st_mode);
   end Permission_Set;

   function Permission_Set (Directory_Entry : Directory_Entry_Type)
      return Permission_Set_Type is
   begin
      Check_Assigned (Directory_Entry);
      return To_Permission_Set (Directory_Entry.State_Data.st_mode);
   end Permission_Set;

end Ada.Directories.Information;
