pragma License (Unrestricted);
--  implementation unit specialized for Linux
with C;
function System.Environment_Block return C.char_ptr_ptr;
pragma Preelaborate (System.Environment_Block);
pragma Inline (System.Environment_Block);
