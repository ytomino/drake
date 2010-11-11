package body Ada.Environment_Variables.Inside is

   environ : C.char_ptr_ptr;
   pragma Import (C, environ);

   function Environment_Block return C.char_ptr_ptr is
   begin
      return environ;
   end Environment_Block;

end Ada.Environment_Variables.Inside;
