pragma License (Unrestricted);
--  AARM A.16(124.b/2), specialized for Windows
private with C.windef;
package Ada.Directories.Information is
   --  System-specific directory information.
   --  Version for the Microsoft(R) Windows(R) operating system.

   function Creation_Time (Name : String) return Calendar.Time;

   function Last_Access_Time (Name : String) return Calendar.Time;

   function Is_Read_Only (Name : String) return Boolean;

   function Needs_Archiving (Name : String) return Boolean;
      --  This generally means that the file needs to be backed up.
      --  The flag is only cleared by backup programs.

   function Is_Compressed (Name : String) return Boolean;

   function Is_Encrypted (Name : String) return Boolean;

   function Is_Hidden (Name : String) return Boolean;

   function Is_System (Name : String) return Boolean;

   function Is_Offline (Name : String) return Boolean;

   function Is_Temporary (Name : String) return Boolean;

   function Is_Sparse (Name : String) return Boolean;

   function Is_Not_Indexed (Name : String) return Boolean;

   function Creation_Time (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Calendar.Time;

   function Last_Access_Time (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Calendar.Time;

   function Is_Read_Only (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   function Needs_Archiving (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;
      --  This generally means that the file needs to be backed up.
      --  The flag is only cleared by backup programs.

   function Is_Compressed (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   function Is_Encrypted (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   function Is_Hidden (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   function Is_System (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   function Is_Offline (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   function Is_Temporary (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   function Is_Sparse (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   function Is_Not_Indexed (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   --  Additional implementation-defined subprograms allowed here.

   --  extended
   function Is_Symbolic_Link (Name : String) return Boolean;
   function Is_Symbolic_Link (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return Boolean;

   --  extended
   --  Unique file identifier.
   type File_Id is private;
   function Identity (Name : String) return File_Id;
   function Identity (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return File_Id;

   --  unimplemented, source-level compatibility with POSIX
   function Owner (Name : String) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Owner (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Group (Name : String) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Group (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Read_Symbolic_Link (Name : String) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Read_Symbolic_Link (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";

private

   type File_Id is record
      --  These should be extend to 128bit for ReFS.
      FileIndexLow : C.windef.DWORD;
      FileIndexHigh : C.windef.DWORD;
      VolumeSerialNumber : C.windef.DWORD;
   end record;

end Ada.Directories.Information;
