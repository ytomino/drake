with Ada.Unchecked_Conversion;
with C.winbase;
package body System.Native_Stack is
   pragma Suppress (All_Checks);
   use type C.size_t;

   A_NTDLL_DLL : aliased constant Wide_String (1 .. 10) :=
      "NTDLL.DLL" & Wide_Character'Val (0);
   C_NTDLL_DLL : aliased C.wchar_t_array (0 .. A_NTDLL_DLL'Length - 1);
   for C_NTDLL_DLL'Address use A_NTDLL_DLL'Address;

   --  implementation

   procedure Get (
      TEB : C.winnt.struct_TEB_ptr := C.winnt.NtCurrentTeb;
      Top, Bottom : out Address)
   is
      function Cast is new
         Ada.Unchecked_Conversion (C.winnt.PVOID, Address);
      function Cast is new
         Ada.Unchecked_Conversion (C.winnt.struct_TEB_ptr, C.winnt.NT_TIB_ptr);
      TIB : constant C.winnt.NT_TIB_ptr := Cast (TEB);
   begin
      Top := Cast (TIB.StackLimit);
      Bottom := Cast (TIB.StackBase);
   end Get;

   function NTDLL return C.windef.HMODULE is
   begin
      return C.winbase.GetModuleHandle (C_NTDLL_DLL (0)'Access);
   end NTDLL;

end System.Native_Stack;
