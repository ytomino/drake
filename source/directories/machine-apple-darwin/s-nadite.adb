with Ada.Exception_Identification.From_Here;
with Ada.Hierarchical_File_Names;
with System.Zero_Terminated_Strings;
with C.errno;
with C.stdlib;
with C.unistd;
package body System.Native_Directories.Temporary is
   use Ada.Exception_Identification.From_Here;
   use type C.char;
   use type C.char_array;
   use type C.char_ptr;
   use type C.signed_int;
   use type C.size_t;

   Temp_Variable : constant C.char_array := "TMPDIR" & C.char'Val (0);
   Temp_Template : constant C.char_array := "ADAXXXXXX" & C.char'Val (0);

   procedure Put_Template (
      Directory : String;
      Template : not null C.char_ptr;
      Length : out C.size_t);
   procedure Put_Template (
      Directory : String;
      Template : not null C.char_ptr;
      Length : out C.size_t)
   is
      Template_A : C.char_array (C.size_t);
      for Template_A'Address use Template.all'Address;
   begin
      if Directory'Length = 0
         or else Ada.Hierarchical_File_Names.Is_Current_Directory_Name (
            Directory)
      then
         Length := 0;
      else
         Zero_Terminated_Strings.To_C (Directory, Template, Length);
         if not Ada.Hierarchical_File_Names.Is_Path_Delimiter (
            Character (Template_A (Length - 1))) -- Length > 0
         then
            Template_A (Length) :=
               C.char (Ada.Hierarchical_File_Names.Default_Path_Delimiter);
            Length := Length + 1;
         end if;
      end if;
      Template_A (Length .. Length + (Temp_Template'Length - 1)) :=
         Temp_Template;
      Length := Length + (Temp_Template'Length - 1); -- exclude NUL
   end Put_Template;

   --  implementation

   function Temporary_Directory return String is
      Temp_Dir : C.char_ptr;
   begin
      Temp_Dir := C.stdlib.getenv (
         Temp_Variable (Temp_Variable'First)'Access);
      if Temp_Dir = null or else Temp_Dir.all = C.char'Val (0) then
         return "."; -- Is_Current_Directory_Name (".") = True
      else
         return Zero_Terminated_Strings.Value (Temp_Dir);
      end if;
   end Temporary_Directory;

   procedure Set_Temporary_Directory (Name : String) is
      C_Name : C.char_array (
         0 ..
         Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      if C.stdlib.setenv (
         Temp_Variable (Temp_Variable'First)'Access,
         C_Name (C_Name'First)'Access,
         1) /= 0
      then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Set_Temporary_Directory;

   function Create_Temporary_File (Directory : String) return String is
      Template : C.char_array (
         0 ..
         Directory'Length * Zero_Terminated_Strings.Expanding
            + 1 -- '/'
            + Temp_Template'Length);
      Length : C.size_t;
   begin
      Put_Template (Directory, Template (0)'Unchecked_Access, Length);
      declare
         Handle : C.signed_int;
      begin
         declare -- mkstemp where
            use C.stdlib; -- Linux, POSIX.1-2008
            use C.unistd; -- Darwin, FreeBSD
         begin
            Handle := mkstemp (Template (0)'Access);
         end;
         if Handle < 0 then
            Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
         end if;
         if C.unistd.close (Handle) < 0 then
            Raise_Exception (IO_Exception_Id (C.errno.errno));
         end if;
      end;
      return Zero_Terminated_Strings.Value (
         Template (0)'Access,
         Length);
   end Create_Temporary_File;

   function Create_Temporary_Directory (Directory : String) return String is
      Template : C.char_array (
         0 ..
         Directory'Length * Zero_Terminated_Strings.Expanding
            + 1 -- '/'
            + Temp_Template'Length);
      Length : C.size_t;
   begin
      Put_Template (Directory, Template (0)'Unchecked_Access, Length);
      declare
         R : C.char_ptr;
      begin
         declare -- mkdtemp where
            use C.stdlib; -- Linux, POSIX.1-2008
            use C.unistd; -- Darwin, FreeBSD
         begin
            R := mkdtemp (Template (0)'Access);
         end;
         if R = null then
            Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
         end if;
      end;
      return Zero_Terminated_Strings.Value (
         Template (0)'Access,
         Length);
   end Create_Temporary_Directory;

end System.Native_Directories.Temporary;
