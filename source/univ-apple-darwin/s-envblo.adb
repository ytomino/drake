with C.crt_externs;
function System.Environment_Block return C.char_ptr_ptr is
   pragma Suppress (All_Checks);
begin
   return C.crt_externs.NSGetEnviron.all;
end System.Environment_Block;
