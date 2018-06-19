with System.Address_To_Named_Access_Conversions;
with System.Growth;
with System.Zero_Terminated_Strings;
with C.errno;
with C.sys.sysctl;
with C.sys.types;
package body System.Program is
   use type C.signed_int;

   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   mib : aliased constant C.signed_int_array (0 .. 3) := (
      C.sys.sysctl.CTL_KERN,
      C.sys.sysctl.KERN_PROC,
      C.sys.sysctl.KERN_PROC_PATHNAME,
      -1); -- implies the current process

   --  implementation

   function Full_Name return String is
      package Holder is
         new Growth.Scoped_Holder (
            C.sys.types.ssize_t,
            Component_Size => C.char_array'Component_Size);
   begin
      Holder.Reserve_Capacity (1024);
      loop
         declare
            Result_Length : aliased C.size_t := C.size_t (Holder.Capacity);
         begin
            if C.sys.sysctl.sysctl (
               mib (0)'Unrestricted_Access, -- const is missing until FreeBSD8
               4,
               C.void_ptr (Holder.Storage_Address),
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
               return Zero_Terminated_Strings.Value (
                  char_ptr_Conv.To_Pointer (Holder.Storage_Address));
            end if;
         end;
         --  growth
         declare
            function Grow is new Growth.Fast_Grow (C.sys.types.ssize_t);
         begin
            Holder.Reserve_Capacity (Grow (Holder.Capacity));
         end;
      end loop;
   end Full_Name;

end System.Program;
