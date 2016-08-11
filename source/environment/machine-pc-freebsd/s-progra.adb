with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.errno;
with C.sys.sysctl;
package body System.Program is
   use type C.signed_int;
   use type C.size_t;

   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   mib : aliased constant C.signed_int_array (0 .. 3) := (
      C.sys.sysctl.CTL_KERN,
      C.sys.sysctl.KERN_PROC,
      C.sys.sysctl.KERN_PROC_PATHNAME,
      -1); -- implies the current process

   --  implementation

   function Full_Name return String is
      procedure Finally (X : in out C.char_ptr);
      procedure Finally (X : in out C.char_ptr) is
      begin
         Standard_Allocators.Free (char_ptr_Conv.To_Address (X));
      end Finally;
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (C.char_ptr, Finally);
      Buffer_Length : C.size_t := 1024;
      Buffer : aliased C.char_ptr :=
         char_ptr_Conv.To_Pointer (
            Standard_Allocators.Allocate (
               Storage_Elements.Storage_Offset (Buffer_Length)));
   begin
      Holder.Assign (Buffer);
      loop
         declare
            Result_Length : aliased C.size_t := Buffer_Length;
         begin
            if C.sys.sysctl.sysctl (
               mib (0)'Unrestricted_Access, -- const is missing until FreeBSD8
               4,
               C.void_ptr (char_ptr_Conv.To_Address (Buffer)),
               Result_Length'Access,
               C.void_const_ptr (Null_Address),
               0) < 0
            then
               case C.errno.errno is
                  when C.errno.ENOMEM =>
                     null; -- retry since the buffer size is too short
                  when others =>
                     raise Program_Error;
               end case;
            else
               return Zero_Terminated_Strings.Value (Buffer);
            end if;
         end;
         Buffer_Length := Buffer_Length * 2;
         Buffer :=
            char_ptr_Conv.To_Pointer (
               Standard_Allocators.Reallocate (
                  char_ptr_Conv.To_Address (Buffer),
                  Storage_Elements.Storage_Offset (Buffer_Length)));
      end loop;
   end Full_Name;

end System.Program;
