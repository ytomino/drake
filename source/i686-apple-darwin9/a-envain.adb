with C.crt_externs;
package body Ada.Environment_Variables.Inside is
   pragma Suppress (All_Checks);

   function Environment_Block return C.char_ptr_ptr is
   begin
      return C.crt_externs.NSGetEnviron.all;
   end Environment_Block;

end Ada.Environment_Variables.Inside;
