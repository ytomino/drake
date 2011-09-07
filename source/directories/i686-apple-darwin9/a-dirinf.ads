pragma License (Unrestricted);
--  Ada 2005, this package defined by Ada 2005 AARM A.16 (124.b/2)
package Ada.Directories.Information is
   --  System-specific directory information.
   --  Unix and similar systems version.

   function Last_Access_Time (Name : String) return Calendar.Time;

   function Last_Status_Change_Time (Name : String) return Calendar.Time;

   type Permission is (
      Others_Execute, Others_Write, Others_Read,
      Group_Execute, Group_Write, Group_Read,
      Owner_Execute, Owner_Write, Owner_Read,
      Set_Group_ID, Set_User_ID);

   type Permission_Set_Type is array (Permission) of Boolean;
   pragma Pack (Permission_Set_Type);

   function Permission_Set (Name : String) return Permission_Set_Type;

   function Owner (Name : String) return String;
   --  Returns the image of the User_Id. If a definition of User_Id
   --  is available, an implementation-defined version of Owner
   --  returning User_Id should also be defined.

   function Group (Name : String) return String;
   --  Returns the image of the User_Id. If a definition of Group_Id
   --  is available, an implementation-defined version of Group
   --  returning Group_Id should also be defined.

   function Is_Block_Special_File (Name : String) return Boolean;

   function Is_Character_Special_File (Name : String) return Boolean;

   function Is_FIFO (Name : String) return Boolean;

   function Is_Symbolic_Link (Name : String) return Boolean;

   function Is_Socket (Name : String) return Boolean;

   function Last_Access_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time;

   function Last_Status_Change_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time;

   function Permission_Set (Directory_Entry : Directory_Entry_Type)
      return Permission_Set_Type;

   function Owner (Directory_Entry : Directory_Entry_Type) return String;
   --  See Owner above.

   function Group (Directory_Entry : Directory_Entry_Type) return String;
   --  See Group above.

   function Is_Block_Special_File (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   function Is_Character_Special_File (
      Directory_Entry : Directory_Entry_Type) return Boolean;

   function Is_FIFO (Directory_Entry : Directory_Entry_Type) return Boolean;

   function Is_Symbolic_Link (
      Directory_Entry : Directory_Entry_Type) return Boolean;

   function Is_Socket (Directory_Entry : Directory_Entry_Type)
      return Boolean;

   --  Additional implementation-defined subprograms allowed here.
end Ada.Directories.Information;
