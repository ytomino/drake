with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with System.Zero_Terminated_Strings;
with C.stdlib;
with C.sys.param;
with C.fcntl;
package body System.Native_IO.Names is
   use Ada.Exception_Identification.From_Here;
   use type C.char_ptr;
   use type C.size_t;

   function strlen (s : not null access constant C.char) return C.size_t
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_strlen";

   package Name_Pointer_Conv is
      new Address_To_Named_Access_Conversions (Name_Character, Name_Pointer);

   --  implementation

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : File_Mode;
      Name : String;
      Out_Name : aliased out Name_Pointer;
      Form : Packed_Form)
   is
      C_Name : aliased Name_String (
         0 .. Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      Open_Ordinary (Method, Handle, Mode, C_Name (0)'Unchecked_Access, Form);
      Out_Name := null;
   end Open_Ordinary;

   procedure Get_Full_Name (
      Handle : Handle_Type;
      Has_Full_Name : in out Boolean;
      Name : in out Name_Pointer;
      Is_Standard : Boolean;
      Raise_On_Error : Boolean)
   is
      Path : aliased C.char_array (0 .. C.sys.param.MAXPATHLEN - 1);
   begin
      if C.fcntl.fcntl (
         Handle,
         C.fcntl.F_GETPATH,
         Path (0)'Access) < 0
      then
         --  Failed, keep Has_Full_Name and Name.
         if Raise_On_Error then
            Raise_Exception (Use_Error'Identity);
         end if;
      else
         declare
            New_Name_Length : constant C.size_t := strlen (Path (0)'Access);
            New_Name : constant Name_Pointer :=
               Name_Pointer_Conv.To_Pointer (
                  Address (C.stdlib.malloc (New_Name_Length + 1))); -- NUL
         begin
            if New_Name = null then
               --  Failed, keep Has_Full_Name and Name.
               if Raise_On_Error then
                  raise Storage_Error;
               end if;
               return; -- error
            end if;
            declare
               New_Name_All : Name_String (0 .. New_Name_Length);
               for New_Name_All'Address use
                  Name_Pointer_Conv.To_Address (New_Name);
            begin
               New_Name_All := Path (0 .. New_Name_Length);
            end;
            if not Is_Standard then
               Free (Name); -- External or External_No_Close
            end if;
            Name := New_Name;
         end;
         Has_Full_Name := True;
      end if;
   end Get_Full_Name;

end System.Native_IO.Names;
