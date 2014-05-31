with C.stdlib;
with C.sys.types;
with C.unistd;
package body System.Termination is
   pragma Suppress (All_Checks);

   procedure Error_Put (S : String) is
      Dummy : C.sys.types.ssize_t;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.unistd.write (
         C.unistd.STDERR_FILENO,
         C.void_const_ptr (S'Address),
         S'Length);
   end Error_Put;

   procedure Error_New_Line is
   begin
      Error_Put ((1 => Character'Val (10)));
   end Error_New_Line;

   procedure Force_Abort is
   begin
      C.stdlib.C_abort;
   end Force_Abort;

   procedure Register_Exit (Handler : not null Exit_Handler) is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      --  atexit requires handler that has C calling-convention,
      --  but Ada procedure having no argument is same as C.
      Dummy := C.stdlib.atexit (Handler);
   end Register_Exit;

end System.Termination;
