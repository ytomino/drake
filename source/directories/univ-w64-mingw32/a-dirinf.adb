with Ada.Directories.Inside;
with Ada.Unchecked_Conversion;
with System.Native_Time;
with C.windef;
with C.winnt;
package body Ada.Directories.Information is
   use type C.windef.DWORD;

   function Cast is new Unchecked_Conversion (Duration, Calendar.Time);

   --  implementation

   function Creation_Time (Name : String) return Calendar.Time is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return Cast (System.Native_Time.To_Time (
         Information.ftCreationTime));
   end Creation_Time;

   function Creation_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time is
   begin
      Check_Assigned (Directory_Entry);
      return Cast (System.Native_Time.To_Time (
         Directory_Entry.Information.ftLastWriteTime));
   end Creation_Time;

   function Last_Access_Time (Name : String) return Calendar.Time is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return Cast (System.Native_Time.To_Time (
         Information.ftLastAccessTime));
   end Last_Access_Time;

   function Last_Access_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time is
   begin
      Check_Assigned (Directory_Entry);
      return Cast (System.Native_Time.To_Time (
         Directory_Entry.Information.ftLastAccessTime));
   end Last_Access_Time;

   function Is_Read_Only (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SPARSE_FILE) /= 0;
   end Is_Read_Only;

   function Is_Read_Only (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_READONLY) /= 0;
   end Is_Read_Only;

   function Needs_Archiving (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_ARCHIVE) /= 0;
   end Needs_Archiving;

   function Needs_Archiving (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_ARCHIVE) /= 0;
   end Needs_Archiving;

   function Is_Compressed (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_COMPRESSED) /= 0;
   end Is_Compressed;

   function Is_Compressed (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_COMPRESSED) /= 0;
   end Is_Compressed;

   function Is_Encrypted (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_ENCRYPTED) /= 0;
   end Is_Encrypted;

   function Is_Encrypted (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_ENCRYPTED) /= 0;
   end Is_Encrypted;

   function Is_Hidden (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_HIDDEN) /= 0;
   end Is_Hidden;

   function Is_Hidden (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_HIDDEN) /= 0;
   end Is_Hidden;

   function Is_System (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SYSTEM) /= 0;
   end Is_System;

   function Is_System (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SYSTEM) /= 0;
   end Is_System;

   function Is_Offline (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_OFFLINE) /= 0;
   end Is_Offline;

   function Is_Offline (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_OFFLINE) /= 0;
   end Is_Offline;

   function Is_Temporary (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_TEMPORARY) /= 0;
   end Is_Temporary;

   function Is_Temporary (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_TEMPORARY) /= 0;
   end Is_Temporary;

   function Is_Sparse (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SPARSE_FILE) /= 0;
   end Is_Sparse;

   function Is_Sparse (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SPARSE_FILE) /= 0;
   end Is_Sparse;

   function Is_Not_Indexed (Name : String) return Boolean is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_NOT_CONTENT_INDEXED) /= 0;
   end Is_Not_Indexed;

   function Is_Not_Indexed (Directory_Entry : Directory_Entry_Type)
      return Boolean is
   begin
      Check_Assigned (Directory_Entry);
      return (Directory_Entry.Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_NOT_CONTENT_INDEXED) /= 0;
   end Is_Not_Indexed;

end Ada.Directories.Information;
