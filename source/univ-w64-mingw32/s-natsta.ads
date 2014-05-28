pragma License (Unrestricted);
--  runtime unit
with C.winnt;
package System.Native_Stack is
   pragma Preelaborate;
   pragma Linker_Options ("-lntdll"); -- for System.Dynamic_Libraries

   procedure Get (
      TEB : C.winnt.struct_TEB_ptr := C.winnt.NtCurrentTeb;
      Top, Bottom : out Address);

end System.Native_Stack;
