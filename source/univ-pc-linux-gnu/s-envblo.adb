with C.unistd;
package body System.Environment_Block is
   pragma Suppress (All_Checks);

   --  implementation

   function Environment_Block return C.char_ptr_ptr is
   begin
      return C.unistd.environ;
   end Environment_Block;

end System.Environment_Block;
