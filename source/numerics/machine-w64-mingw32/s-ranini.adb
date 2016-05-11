with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with C.winbase;
with C.wincrypt;
with C.windef;
with C.winerror;
package body System.Random_Initiators is
   use Ada.Exception_Identification.From_Here;
   use type C.windef.WINBOOL;
   use type C.windef.DWORD; -- error code

   procedure Get (
      Item : Address;
      Size : Storage_Elements.Storage_Count)
   is
      package BYTE_ptr_Conv is
         new Address_To_Named_Access_Conversions (
            C.windef.BYTE,
            C.windef.BYTE_ptr);
      Context : aliased C.wincrypt.HCRYPTPROV;
      Error : Boolean;
   begin
      if C.wincrypt.CryptAcquireContext (
         Context'Access,
         null,
         null,
         C.wincrypt.PROV_RSA_FULL,
         C.wincrypt.CRYPT_VERIFYCONTEXT) = 0
      then
         Raise_Exception (Use_Error'Identity);
      end if;
      for I in 1 .. 5 loop
         Error := C.wincrypt.CryptGenRandom (
            Context,
            C.windef.DWORD (Size),
            BYTE_ptr_Conv.To_Pointer (Item)) = 0;
         exit when not Error
            or else C.winbase.GetLastError /= C.winerror.ERROR_BUSY;
         C.winbase.Sleep (10); -- ???
      end loop;
      if C.wincrypt.CryptReleaseContext (Context, 0) = 0 then
         Error := True;
      end if;
      if Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Get;

end System.Random_Initiators;
