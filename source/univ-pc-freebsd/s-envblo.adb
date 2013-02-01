package body System.Environment_Block is
   pragma Suppress (All_Checks);

   environ : C.char_ptr_ptr;
   pragma Import (C, environ);

   --  implementation

   function Environment_Block return C.char_ptr_ptr is
   begin
      return environ;
   end Environment_Block;

end System.Environment_Block;
