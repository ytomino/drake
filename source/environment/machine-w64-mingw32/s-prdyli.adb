with Ada.Exception_Identification.From_Here;
with Ada.Unchecked_Conversion;
with System.Zero_Terminated_WStrings;
with C.winbase;
with C.winnt;
package body System.Program.Dynamic_Linking is
   use Ada.Exception_Identification.From_Here;
   use type C.size_t;
   use type C.windef.FARPROC;
   use type C.windef.HMODULE;
   use type C.windef.WINBOOL;

   procedure memcpy (dst, src : Address; n : C.size_t)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memcpy";

   procedure Open (Handle : out C.windef.HMODULE; Name : String);
   procedure Open (Handle : out C.windef.HMODULE; Name : String) is
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      Handle := C.winbase.LoadLibrary (W_Name (0)'Access);
      if Handle = null then
         Raise_Exception (Name_Error'Identity);
      end if;
   end Open;

   procedure Close (Handle : C.windef.HMODULE; Raise_On_Error : Boolean);
   procedure Close (Handle : C.windef.HMODULE; Raise_On_Error : Boolean) is
      Success : C.windef.WINBOOL;
   begin
      Success := C.winbase.FreeLibrary (Handle);
      if Success = C.windef.FALSE and then Raise_On_Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Close;

   --  implementation

   function Is_Open (Lib : Library) return Boolean is
      Handle : C.windef.HMODULE
         renames Controlled.Reference (Lib).all;
   begin
      return Handle /= null;
   end Is_Open;

   procedure Open (Lib : in out Library; Name : String) is
      pragma Check (Pre,
         Check => not Is_Open (Lib) or else raise Status_Error);
      Handle : C.windef.HMODULE
         renames Controlled.Reference (Lib).all;
   begin
      Open (Handle, Name);
   end Open;

   function Open (Name : String) return Library is
   begin
      return Result : Library do
         declare
            Handle : C.windef.HMODULE
               renames Controlled.Reference (Result).all;
         begin
            Open (Handle, Name);
         end;
      end return;
   end Open;

   procedure Close (Lib : in out Library) is
      pragma Check (Pre,
         Check => Is_Open (Lib) or else raise Status_Error);
      Handle : C.windef.HMODULE
         renames Controlled.Reference (Lib).all;
      Freeing_Handle : constant C.windef.HMODULE := Handle;
   begin
      Handle := null;
      Close (Freeing_Handle, Raise_On_Error => True);
   end Close;

   function Import (
      Lib : Library;
      Symbol : String)
      return Address
   is
      pragma Check (Pre,
         Check => Is_Open (Lib) or else raise Status_Error);
      function Cast is
         new Ada.Unchecked_Conversion (C.windef.FARPROC, Address);
      Handle : C.windef.HMODULE
         renames Controlled.Reference (Lib).all;
      C_Symbol_Length : constant C.size_t := Symbol'Length;
      C_Symbol : aliased C.char_array (0 .. C_Symbol_Length); -- NUL
      Result : C.windef.FARPROC;
   begin
      memcpy (C_Symbol'Address, Symbol'Address, C_Symbol_Length);
      C_Symbol (C_Symbol_Length) := C.char'Val (0);
      Result := C.winbase.GetProcAddress (Handle, C_Symbol (0)'Access);
      if Result = null then
         Raise_Exception (Data_Error'Identity);
      else
         return Cast (Result);
      end if;
   end Import;

   package body Controlled is

      function Reference (Lib : Dynamic_Linking.Library)
         return not null access C.windef.HMODULE is
      begin
         return Library (Lib).Handle'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Library) is
      begin
         if Object.Handle /= null then
            Close (Object.Handle, Raise_On_Error => False);
         end if;
      end Finalize;

   end Controlled;

end System.Program.Dynamic_Linking;
