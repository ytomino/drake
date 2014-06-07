with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.winbase;
with C.windef;
with C.winnt;
package body System.Program is
   use type System.Storage_Elements.Storage_Offset;
   use type C.windef.DWORD;

   function Full_Name return String is
      package Conv is
         new Address_To_Named_Access_Conversions (
            C.winnt.WCHAR,
            C.winnt.LPWSTR);
      procedure Finally (X : not null access C.winnt.LPWSTR);
      procedure Finally (X : not null access C.winnt.LPWSTR) is
      begin
         Standard_Allocators.Free (Conv.To_Address (X.all));
      end Finally;
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (C.winnt.LPWSTR, Finally);
      Buffer_Length : C.windef.DWORD := 1024;
      Buffer : aliased C.winnt.LPWSTR :=
         Conv.To_Pointer (Standard_Allocators.Allocate (
            Storage_Elements.Storage_Count (Buffer_Length)
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
   begin
      Holder.Assign (Buffer'Access);
      loop
         declare
            Result_Length : C.windef.DWORD;
         begin
            Result_Length := C.winbase.GetModuleFileName (
               null,
               Buffer,
               Buffer_Length);
            if Result_Length < Buffer_Length then
               return Zero_Terminated_WStrings.Value (
                  Buffer,
                  C.size_t (Result_Length));
            end if;
         end;
         Buffer_Length := Buffer_Length * 2;
         Buffer := Conv.To_Pointer (
            Standard_Allocators.Reallocate (
               Conv.To_Address (Buffer),
               Storage_Elements.Storage_Count (Buffer_Length)
                  * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      end loop;
   end Full_Name;

end System.Program;
