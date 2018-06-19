with System.Address_To_Named_Access_Conversions;
with System.Growth;
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
      package Holder is
         new Growth.Scoped_Holder (
            C.sys.types.ssize_t,
            Component_Size => C.char_array'Component_Size);
   begin
      Holder.Reserve_Capacity (1024);
      loop
         declare
            S_Length : C.sys.types.ssize_t;
         begin
            S_Length :=
               C.unistd.readlink (
                  Name,
                  char_ptr_Conv.To_Pointer (Holder.Storage_Address),
                  C.size_t (Holder.Capacity));
            if S_Length < 0 then
               raise Program_Error;
            end if;
            if S_Length < Holder.Capacity then
               return Zero_Terminated_Strings.Value (
                  char_ptr_Conv.To_Pointer (Holder.Storage_Address),
                  C.size_t (S_Length));
            end if;
         end;
         --  growth
         declare
            function Grow is new Growth.Fast_Grow (C.sys.types.ssize_t);
         begin
            Holder.Reserve_Capacity (Grow (Holder.Capacity));
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
