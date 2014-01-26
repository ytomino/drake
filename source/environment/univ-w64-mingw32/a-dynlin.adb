with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with System.Zero_Terminated_WStrings;
with C.winbase;
with C.winnt;
package body Ada.Dynamic_Linking is
   use type C.size_t;
   use type C.windef.FARPROC;
   use type C.windef.HMODULE;

   procedure Open (Handle : out C.windef.HMODULE; Name : String);
   procedure Open (Handle : out C.windef.HMODULE; Name : String) is
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * System.Zero_Terminated_WStrings.Expanding);
      Result : C.windef.HMODULE;
   begin
      System.Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      Result := C.winbase.LoadLibrary (W_Name (0)'Access);
      if Result = null then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      else
         Handle := Result;
      end if;
   end Open;

   procedure Close (Handle : C.windef.HMODULE);
   procedure Close (Handle : C.windef.HMODULE) is
      Dummy : C.windef.WINBOOL;
      pragma Unreferenced (Dummy);
   begin
      if Handle /= null then
         Dummy := C.winbase.FreeLibrary (Handle);
      end if;
   end Close;

   --  implementation

   procedure Open (Lib : in out Library; Name : String) is
      Handle : constant not null access C.windef.HMODULE := Reference (Lib);
   begin
      if Handle.all /= null then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
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
      Handle : constant not null access C.windef.HMODULE := Reference (Lib);
   begin
      Close (Handle.all);
      Handle.all := null;
   end Close;

   function Is_Open (Lib : Library) return Boolean is
   begin
      return Reference (Lib).all /= null;
   end Is_Open;

   function Import (Lib : Library; Symbol : String) return System.Address is
      Handle : constant C.windef.HMODULE := Reference (Lib).all;
   begin
      if Handle = null then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      else
         declare
            function Cast is
               new Unchecked_Conversion (C.windef.FARPROC, System.Address);
            Z_Symbol : String := Symbol & Character'Val (0);
            C_Symbol : C.char_array (C.size_t);
            for C_Symbol'Address use Z_Symbol'Address;
            Result : C.windef.FARPROC;
         begin
            Result := C.winbase.GetProcAddress (Handle, C_Symbol (0)'Access);
            if Result = null then
               Exceptions.Raise_Exception_From_Here (Data_Error'Identity);
            else
               return Cast (Result);
            end if;
         end;
      end if;
   end Import;

   package body Controlled is

      function Reference (Lib : Library)
         return not null access C.windef.HMODULE is
      begin
         return Lib.Handle'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Library) is
      begin
         Close (Object.Handle);
      end Finalize;

   end Controlled;

end Ada.Dynamic_Linking;
