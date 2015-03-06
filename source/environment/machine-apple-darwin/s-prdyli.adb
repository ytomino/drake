with Ada.Exception_Identification.From_Here;
with System.Zero_Terminated_Strings;
with C.dlfcn;
package body System.Program.Dynamic_Linking is
   pragma Suppress (All_Checks);
   use Ada.Exception_Identification.From_Here;
   use type C.signed_int;
   use type C.size_t;

   procedure Open (Handle : out C.void_ptr; Name : String);
   procedure Open (Handle : out C.void_ptr; Name : String) is
      C_Name : C.char_array (
         0 ..
         Name'Length * Zero_Terminated_Strings.Expanding);
      Result : C.void_ptr;
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      Result := C.dlfcn.dlopen (C_Name (0)'Access, 0);
      if Address (Result) = Null_Address then
         Raise_Exception (Name_Error'Identity);
      else
         Handle := Result;
      end if;
   end Open;

   procedure Close (Handle : C.void_ptr; Raise_On_Error : Boolean);
   procedure Close (Handle : C.void_ptr; Raise_On_Error : Boolean) is
      R : C.signed_int;
   begin
      if Address (Handle) /= Null_Address then
         R := C.dlfcn.dlclose (Handle);
         if R < 0 and then Raise_On_Error then
            Raise_Exception (Use_Error'Identity);
         end if;
      end if;
   end Close;

   --  implementation

   procedure Open (Lib : in out Library; Name : String) is
      Handle : constant not null access C.void_ptr := Reference (Lib);
   begin
      if Address (Handle.all) /= Null_Address then
         Raise_Exception (Status_Error'Identity);
      else
         Open (Handle.all, Name);
      end if;
   end Open;

   function Open (Name : String) return Library is
   begin
      return Result : Library do
         Open (Reference (Result).all, Name);
      end return;
   end Open;

   procedure Close (Lib : in out Library) is
      Handle : constant not null access C.void_ptr := Reference (Lib);
   begin
      Close (Handle.all, Raise_On_Error => True);
      Handle.all := C.void_ptr (Null_Address);
   end Close;

   function Is_Open (Lib : Library) return Boolean is
   begin
      return Address (Reference (Lib).all) /= Null_Address;
   end Is_Open;

   function Import (Lib : Library; Symbol : String) return Address is
      Handle : constant C.void_ptr := Reference (Lib).all;
   begin
      if Address (Handle) = Null_Address then
         Raise_Exception (Status_Error'Identity);
      else
         declare
            C_Symbol : C.char_array (
               0 ..
               Symbol'Length * Zero_Terminated_Strings.Expanding);
            Result : C.void_ptr;
         begin
            Zero_Terminated_Strings.To_C (Symbol, C_Symbol (0)'Access);
            Result := C.dlfcn.dlsym (Handle, C_Symbol (0)'Access);
            if Address (Result) = Null_Address then
               Raise_Exception (Data_Error'Identity);
            else
               return Address (Result);
            end if;
         end;
      end if;
   end Import;

   package body Controlled is

      function Reference (Lib : Library) return not null access C.void_ptr is
      begin
         return Lib.Handle'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Library) is
      begin
         Close (Object.Handle, Raise_On_Error => False);
      end Finalize;

   end Controlled;

end System.Program.Dynamic_Linking;
