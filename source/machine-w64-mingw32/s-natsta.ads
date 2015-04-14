pragma License (Unrestricted);
--  runtime unit specialized for Windows
with C.windef;
with C.winnt;
package System.Native_Stack is
   pragma Preelaborate;

   procedure Get (
      TEB : C.winnt.struct_TEB_ptr := C.winnt.NtCurrentTeb;
      Top, Bottom : out Address);

   --  helper
   function NTDLL return C.windef.HMODULE;

end System.Native_Stack;
