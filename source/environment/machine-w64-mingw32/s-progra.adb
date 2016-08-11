with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.winbase;
with C.windef;
with C.winnt;
package body System.Program is
   use type Storage_Elements.Storage_Offset;
   use type C.windef.DWORD;

   package LPWSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.WCHAR, C.winnt.LPWSTR);

   --  implementation

   function Full_Name return String is
      procedure Finally (X : in out C.winnt.LPWSTR);
      procedure Finally (X : in out C.winnt.LPWSTR) is
      begin
         Standard_Allocators.Free (LPWSTR_Conv.To_Address (X));
      end Finally;
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (C.winnt.LPWSTR, Finally);
      Buffer_Length : C.windef.DWORD := 1024;
      Buffer : aliased C.winnt.LPWSTR :=
         LPWSTR_Conv.To_Pointer (
            Standard_Allocators.Allocate (
               Storage_Elements.Storage_Offset (Buffer_Length)
                  * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
   begin
      Holder.Assign (Buffer);
      loop
         declare
            Result_Length : C.windef.DWORD;
         begin
            Result_Length :=
               C.winbase.GetModuleFileName (null, Buffer, Buffer_Length);
            if Result_Length < Buffer_Length then
               return Zero_Terminated_WStrings.Value (
                  Buffer,
                  C.size_t (Result_Length));
            end if;
         end;
         Buffer_Length := Buffer_Length * 2;
         Buffer :=
            LPWSTR_Conv.To_Pointer (
               Standard_Allocators.Reallocate (
                  LPWSTR_Conv.To_Address (Buffer),
                  Storage_Elements.Storage_Offset (Buffer_Length)
                     * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      end loop;
   end Full_Name;

end System.Program;
