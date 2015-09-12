with C.stdlib;
with C.sys.types;
with C.sys.uio;
with C.unistd;
package body System.Termination is
   pragma Suppress (All_Checks);

   New_Line : aliased constant C.char := C.char'Val (10);

   procedure Error_Put_Line (S : String) is
      iovec : aliased array (0 .. 1) of aliased C.sys.uio.struct_iovec := (
         (C.void_ptr (S'Address), S'Length),
         (C.void_ptr (New_Line'Address), 1));
      Dummy : C.sys.types.ssize_t;
   begin
      Dummy := C.sys.uio.writev (
         C.unistd.STDERR_FILENO,
         iovec (0)'Access,
         iovec'Length);
   end Error_Put_Line;

   procedure Force_Abort is
   begin
      C.stdlib.C_abort;
   end Force_Abort;

   procedure Register_Exit (Handler : not null Exit_Handler) is
      Dummy : C.signed_int;
   begin
      --  atexit requires handler that has C calling-convention,
      --  but Ada procedure having no argument is same as C.
      Dummy := C.stdlib.atexit (Handler);
   end Register_Exit;

end System.Termination;
