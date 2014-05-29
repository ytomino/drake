with Ada.Unchecked_Conversion;
package body System.Native_Stack is
   pragma Suppress (All_Checks);

   procedure Get (
      TEB : C.winnt.struct_TEB_ptr := C.winnt.NtCurrentTeb;
      Top, Bottom : out Address)
   is
      function Cast is new
         Ada.Unchecked_Conversion (C.winnt.struct_TEB_ptr, C.winnt.NT_TIB_ptr);
      TIB : constant C.winnt.NT_TIB_ptr := Cast (TEB);
   begin
      Top := Address (TIB.StackLimit);
      Bottom := Address (TIB.StackBase);
   end Get;

end System.Native_Stack;
