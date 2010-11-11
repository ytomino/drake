with System.To_String;
with C.stdlib;
with C.unistd;
package body Ada.Directories.Temporary is
   use type C.char_array;
   use type C.char_ptr;
   use type C.signed_int;

   Temp_Variable : constant C.char_array := "TMPDIR" & C.char'Val (0);
   Temp_Template : constant String := "ADAXXXXXX";

   function Temporary_Directory return String is
      Temp_Dir : C.char_ptr;
   begin
      Temp_Dir := C.stdlib.getenv (Temp_Variable (1)'Unrestricted_Access);
      if Temp_Dir = null then
         return Current_Directory;
      else
         return System.To_String (Temp_Dir.all'Address);
      end if;
   end Temporary_Directory;

   function Create_Temporary_File (
      Directory : String := Temporary_Directory) return String
   is
      Template : String (1 .. Directory'Length + Temp_Template'Length + 2);
      Last : Integer := Directory'Length;
   begin
      Template (1 .. Last) := Directory;
      Include_Trailing_Path_Delimiter (Template, Last);
      Template (Last + 1 .. Last + Temp_Template'Length) := Temp_Template;
      Last := Last + Temp_Template'Length;
      Template (Last + 1) := Character'Val (0);
      declare
         C_Template : aliased C.char_array (0 .. Template'Length);
         for C_Template'Address use Template'Address;
         Handle : C.signed_int;
         Dummy : C.signed_int;
         pragma Unreferenced (Dummy);
      begin
         Handle := C.unistd.mkstemp (C_Template (0)'Access);
         if Handle < 0 then
            raise Use_Error;
         end if;
         Dummy := C.unistd.close (Handle);
      end;
      return Template (1 .. Last);
   end Create_Temporary_File;

   function Create_Temporary_Directory (
      Directory : String := Temporary_Directory) return String
   is
      Template : String (1 .. Directory'Length + Temp_Template'Length + 2);
      Last : Integer := Directory'Length;
   begin
      Template (1 .. Last) := Directory;
      Include_Trailing_Path_Delimiter (Template, Last);
      Template (Last + 1 .. Last + Temp_Template'Length) := Temp_Template;
      Last := Last + Temp_Template'Length;
      Template (Last + 1) := Character'Val (0);
      declare
         C_Template : aliased C.char_array (0 .. Template'Length);
         for C_Template'Address use Template'Address;
      begin
         if C.unistd.mkdtemp (C_Template (0)'Access) = null then
            raise Use_Error;
         end if;
      end;
      return Template (1 .. Last);
   end Create_Temporary_Directory;

end Ada.Directories.Temporary;
