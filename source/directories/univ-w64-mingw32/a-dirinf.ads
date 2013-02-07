pragma License (Unrestricted);
--  Ada 2005, this package defined by Ada 2005 AARM A.16 (124.b/2)
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

   function Creation_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time;

   function Last_Access_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time;

   function Is_Read_Only (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   function Needs_Archiving (Directory_Entry : Directory_Entry_Type)
      return Boolean;
   --  This generally means that the file needs to be backed up.
   --  The flag is only cleared by backup programs.

   function Is_Compressed (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   function Is_Encrypted (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   function Is_Hidden (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   function Is_System (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   function Is_Offline (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   function Is_Temporary (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   function Is_Sparse (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   function Is_Not_Indexed (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   --  Additional implementation-defined subprograms allowed here.

   --  unimplemented, source-level compatibility with POSIX
   function Owner (Name : String) return String;
   function Owner (Directory_Entry : Directory_Entry_Type) return String;
   function Group (Name : String) return String;
   function Group (Directory_Entry : Directory_Entry_Type) return String;
   function Read_Symbolic_Link (Name : String) return String;
   function Read_Symbolic_Link (Directory_Entry : Directory_Entry_Type)
      return String;
   pragma Import (Ada, Owner, "__drake_program_error");
   pragma Import (Ada, Group, "__drake_program_error");
   pragma Import (Ada, Read_Symbolic_Link, "__drake_program_error");

end Ada.Directories.Information;
