with System.Address_To_Named_Access_Conversions;
with C.dlfcn;
with C.sys.link_elf;
package body System.Runtime_Information is
   pragma Suppress (All_Checks);
   use type C.signed_int;

   function Load_Address return Address is
      package Conv is
         new Address_To_Named_Access_Conversions (
            C.char,
            C.char_ptr);
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
      return Conv.To_Address (Map.l_addr);
   end Load_Address;

end System.Runtime_Information;
