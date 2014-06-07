with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.mach_o.dyld;
with C.stdint;
package body System.Program is
   use type C.signed_int;
   use type C.stdint.uint32_t;

   function Full_Name return String is
      package Conv is
         new Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      procedure Finally (X : not null access C.char_ptr);
      procedure Finally (X : not null access C.char_ptr) is
      begin
         Standard_Allocators.Free (Conv.To_Address (X.all));
      end Finally;
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (C.char_ptr, Finally);
      Buffer_Length : C.stdint.uint32_t := 1024;
      Buffer : aliased C.char_ptr :=
         Conv.To_Pointer (Standard_Allocators.Allocate (
            Storage_Elements.Storage_Count (Buffer_Length)));
   begin
      Holder.Assign (Buffer'Access);
      loop
         declare
            Result_Length : aliased C.stdint.uint32_t := Buffer_Length;
         begin
            if C.mach_o.dyld.NSGetExecutablePath ( -- or use proc_pidpath ?
               Buffer,
               Result_Length'Access) = 0
            then
               return Zero_Terminated_Strings.Value (
                  Buffer,
                  C.size_t (Result_Length));
            end if;
         end;
         Buffer_Length := Buffer_Length * 2;
         Buffer := Conv.To_Pointer (
            Standard_Allocators.Reallocate (
               Conv.To_Address (Buffer),
               Storage_Elements.Storage_Count (Buffer_Length)));
      end loop;
   end Full_Name;

end System.Program;
