function System.Environment_Block return C.char_ptr_ptr is
   environ : C.char_ptr_ptr
      with Import, Convention => C;
   pragma Unmodified (environ);
   --  [gcc-4.9] does not take Import as initialized
begin
   return environ;
end System.Environment_Block;
