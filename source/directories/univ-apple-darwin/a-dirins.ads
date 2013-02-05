pragma License (Unrestricted);
--  implementation unit
with System.Native_Time;
with C.sys.stat;
package Ada.Directories.Inside is

   function Current_Directory return String;

   procedure Set_Directory (Directory : String);

   procedure Create_Directory (
      New_Directory : String;
      Form : String);

   procedure Delete_Directory (Directory : String);

   procedure Delete_File (Name : String);

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Form : String;
      Overwrite : Boolean);

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean);

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean);

   function Exists (Name : String) return Boolean;

   --  same as Directory_Searching.Directory_Entry_Information_Type
   subtype Directory_Entry_Information_Type is C.sys.stat.struct_stat;

   procedure Get_Information (
      Name : String;
      Information : not null access Directory_Entry_Information_Type);

   function Kind (Information : Directory_Entry_Information_Type)
      return File_Kind;

   function Size (Information : Directory_Entry_Information_Type)
      return File_Size;

   function Modification_Time (Information : Directory_Entry_Information_Type)
      return System.Native_Time.Native_Time;

   procedure Set_Modification_Time (
      Name : String;
      Time : System.Native_Time.Native_Time);

end Ada.Directories.Inside;
