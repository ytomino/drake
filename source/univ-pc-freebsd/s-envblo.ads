pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD
with C;
package System.Environment_Block is
   pragma Preelaborate;

   function Environment_Block return C.char_ptr_ptr;

end System.Environment_Block;
