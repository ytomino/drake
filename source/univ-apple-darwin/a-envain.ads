pragma License (Unrestricted);
--  implementation unit
with C;
package Ada.Environment_Variables.Inside is
   pragma Preelaborate;

   function Environment_Block return C.char_ptr_ptr;

end Ada.Environment_Variables.Inside;