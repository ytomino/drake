with Ada.Exceptions;
with C.dlfcn;
package body Ada.Dynamic_Linking is
   use type C.void_ptr;

   procedure Close (Handle : C.void_ptr);
   procedure Close (Handle : C.void_ptr) is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      if Handle /= C.void_ptr (System.Null_Address) then
         Dummy := C.dlfcn.dlclose (Handle);
      end if;
   end Close;

   --  implementation

   procedure Open (Lib : in out Library; Name : String) is
      Handle : constant not null access C.void_ptr := Reference (Lib);
   begin
      if Handle.all /= C.void_ptr (System.Null_Address) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      else
         declare
            Z_Name : String := Name & Character'Val (0);
            C_Name : C.char_array (C.size_t);
            for C_Name'Address use Z_Name'Address;
            Result : C.void_ptr;
         begin
            Result := C.dlfcn.dlopen (C_Name (0)'Access, 0);
            if Result = C.void_ptr (System.Null_Address) then
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            else
               Handle.all := Result;
            end if;
         end;
      end if;
   end Open;

   function Open (Name : String) return Library is
   begin
      return Result : Library do
         Open (Result, Name);
      end return;
   end Open;

   procedure Close (Lib : in out Library) is
      Handle : constant not null access C.void_ptr := Reference (Lib);
   begin
      Close (Handle.all);
      Handle.all := C.void_ptr (System.Null_Address);
   end Close;

   function Is_Open (Lib : Library) return Boolean is
   begin
      return Reference (Lib).all /= C.void_ptr (System.Null_Address);
   end Is_Open;

   function Import (Lib : Library; Symbol : String) return System.Address is
      Handle : constant C.void_ptr := Reference (Lib).all;
   begin
      if Handle = C.void_ptr (System.Null_Address) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      else
         declare
            Z_Symbol : String := Symbol & Character'Val (0);
            C_Symbol : C.char_array (C.size_t);
            for C_Symbol'Address use Z_Symbol'Address;
            Result : C.void_ptr;
         begin
            Result := C.dlfcn.dlsym (Handle, C_Symbol (0)'Access);
            if Result = C.void_ptr (System.Null_Address) then
               Exceptions.Raise_Exception_From_Here (Data_Error'Identity);
            else
               return System.Address (Result);
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
         Close (Object.Handle);
      end Finalize;

   end Controlled;

end Ada.Dynamic_Linking;
