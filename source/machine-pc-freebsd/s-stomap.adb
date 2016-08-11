with System.Address_To_Named_Access_Conversions;
with C.dlfcn;
with C.sys.link_elf;
package body System.Storage_Map is
   pragma Suppress (All_Checks);
   use type C.signed_int;

   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   --  implementation

   function Load_Address return Address is
      type Link_map_ptr is access C.sys.link_elf.Link_map
         with Convention => C;
      for Link_map_ptr'Storage_Size use 0;
      Map : aliased Link_map_ptr;
   begin
      if C.dlfcn.dlinfo (
         C.dlfcn.RTLD_SELF,
         C.dlfcn.RTLD_DI_LINKMAP,
         C.void_ptr (Map'Address)) < 0
      then
         return Null_Address; -- ???
      end if;
      return char_ptr_Conv.To_Address (Map.l_addr);
   end Load_Address;

end System.Storage_Map;
