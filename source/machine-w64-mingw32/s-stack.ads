pragma License (Unrestricted);
--  runtime unit specialized for Windows
with C.winnt;
package System.Stack is
   pragma Preelaborate;

   procedure Get (
      TEB : C.winnt.struct_TEB_ptr := C.winnt.NtCurrentTeb;
      Top, Bottom : out Address);

end System.Stack;
