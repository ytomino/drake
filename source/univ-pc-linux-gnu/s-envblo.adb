with C.unistd;
function System.Environment_Block return C.char_ptr_ptr is
   pragma Suppress (All_Checks);
begin
   return C.unistd.environ;
end System.Environment_Block;
