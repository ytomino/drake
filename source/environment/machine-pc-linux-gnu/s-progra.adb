with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.unistd;
with C.sys.types;
package body System.Program is
   pragma Suppress (All_Checks);
   use type C.char_array;
   use type C.size_t;
   use type C.sys.types.ssize_t;

   function Read_Symbolic_Link (Name : not null access constant C.char)
      return String;
   function Read_Symbolic_Link (Name : not null access constant C.char)
      return String
   is
      package Conv is
         new Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      procedure Finally (X : not null access C.char_ptr);
      procedure Finally (X : not null access C.char_ptr) is
      begin
         Standard_Allocators.Free (Conv.To_Address (X.all));
      end Finally;
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (C.char_ptr, Finally);
      Buffer_Length : C.size_t := 1024;
      Buffer : aliased C.char_ptr :=
         Conv.To_Pointer (Standard_Allocators.Allocate (
            Storage_Elements.Storage_Count (Buffer_Length)));
   begin
      Holder.Assign (Buffer'Access);
      loop
         declare
            Result : constant C.sys.types.ssize_t := C.unistd.readlink (
               Name,
               Buffer,
               Buffer_Length);
         begin
            if Result < 0 then
               raise Program_Error;
            end if;
            if C.size_t (Result) < Buffer_Length then
               return Zero_Terminated_Strings.Value (
                  Buffer,
                  C.size_t (Result));
            end if;
            Buffer_Length := Buffer_Length * 2;
            Buffer := Conv.To_Pointer (
               Standard_Allocators.Reallocate (
                  Conv.To_Address (Buffer),
                  Storage_Elements.Storage_Count (Buffer_Length)));
         end;
      end loop;
   end Read_Symbolic_Link;

   proc_self_exe : aliased constant C.char_array (0 .. 14) :=
      "/proc/self/exe" & C.char'Val (0);

   --  implementation

   function Full_Name return String is
   begin
      return Read_Symbolic_Link (proc_self_exe (0)'Access);
   end Full_Name;

end System.Program;
