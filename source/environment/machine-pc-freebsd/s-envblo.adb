function System.Environment_Block return C.char_ptr_ptr is
   environ : C.char_ptr_ptr
      with Import, Convention => C;
begin
   return environ;
end System.Environment_Block;
