with C.unistd;
function System.Environment_Block return C.char_ptr_ptr is
begin
   return C.unistd.environ;
end System.Environment_Block;
