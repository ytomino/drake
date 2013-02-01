with C.crt_externs;
package body System.Environment_Block is
   pragma Suppress (All_Checks);

   function Environment_Block return C.char_ptr_ptr is
   begin
      return C.crt_externs.NSGetEnviron.all;
   end Environment_Block;

end System.Environment_Block;
