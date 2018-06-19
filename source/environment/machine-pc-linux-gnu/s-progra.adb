with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Growth;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.sys.types;
with C.unistd;
package body System.Program is
   use type C.char_array;
   use type C.sys.types.ssize_t;

   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   function Read_Symbolic_Link (Name : not null access constant C.char)
      return String;
   function Read_Symbolic_Link (Name : not null access constant C.char)
      return String
   is
      procedure Finally (X : in out C.char_ptr);
      procedure Finally (X : in out C.char_ptr) is
      begin
         Standard_Allocators.Free (char_ptr_Conv.To_Address (X));
      end Finally;
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (C.char_ptr, Finally);
      Buffer_Length : C.sys.types.ssize_t := 1024;
      Buffer : aliased C.char_ptr :=
         char_ptr_Conv.To_Pointer (
            Standard_Allocators.Allocate (
               Storage_Elements.Storage_Offset (Buffer_Length)));
   begin
      Holder.Assign (Buffer);
      loop
         declare
            S_Length : C.sys.types.ssize_t;
         begin
            S_Length :=
               C.unistd.readlink (Name, Buffer, C.size_t (Buffer_Length));
            if S_Length < 0 then
               raise Program_Error;
            end if;
            if S_Length < Buffer_Length then
               return Zero_Terminated_Strings.Value (
                  Buffer,
                  C.size_t (S_Length));
            end if;
         end;
         --  growth
         declare
            function Grow is new Growth.Fast_Grow (C.sys.types.ssize_t);
         begin
            Buffer_Length := Grow (Buffer_Length);
         end;
         Buffer :=
            char_ptr_Conv.To_Pointer (
               Standard_Allocators.Reallocate (
                  char_ptr_Conv.To_Address (Buffer),
                  Storage_Elements.Storage_Offset (Buffer_Length)));
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
