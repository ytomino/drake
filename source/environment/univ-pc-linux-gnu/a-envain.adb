with C.unistd;
package body Ada.Environment_Variables.Inside is
   pragma Suppress (All_Checks);

   --  implementation

   function Environment_Block return C.char_ptr_ptr is
   begin
      return C.unistd.environ;
   end Environment_Block;

end Ada.Environment_Variables.Inside;
